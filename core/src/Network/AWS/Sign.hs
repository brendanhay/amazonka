

data Signer = Signer

class AWSSigner v where
    signed :: AuthEnv
           -> Region
           -> UTCTime
           -> Service
           -> Request a
           -> Signed v a

class AWSPresigner v where
    presigned :: AuthEnv
              -> Region
              -> UTCTime
              -> Seconds
              -> Service
              -> Request a
              -> Signed v a
