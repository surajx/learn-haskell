module MyTypes where
data Voter a = MkVoter String String
aVoter :: Voter a
aVoter = MkVoter "Alan Turing" "Milton Keynes, UK"


