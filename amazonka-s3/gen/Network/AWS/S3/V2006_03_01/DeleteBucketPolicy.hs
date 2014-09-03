{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the policy from the bucket.
module Network.AWS.S3.V2006_03_01.DeleteBucketPolicy
    (
    -- * Request
      DeleteBucketPolicy
    -- ** Request constructor
    , deleteBucketPolicy
    -- ** Request lenses
    , dbprBucket

    -- * Response
    , DeleteBucketPolicyResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteBucketPolicy' request.
deleteBucketPolicy :: BucketName -- ^ 'dbprBucket'
                   -> DeleteBucketPolicy
deleteBucketPolicy p1 = DeleteBucketPolicy
    { _dbprBucket = p1
    }

data DeleteBucketPolicy = DeleteBucketPolicy
    { _dbprBucket :: BucketName
    } deriving (Show, Generic)

dbprBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> DeleteBucketPolicy
    -> f DeleteBucketPolicy
dbprBucket f x =
    (\y -> x { _dbprBucket = y })
       <$> f (_dbprBucket x)
{-# INLINE dbprBucket #-}

instance ToPath DeleteBucketPolicy where
    toPath DeleteBucketPolicy{..} = mconcat
        [ "/"
        , toBS _dbprBucket
        ]

instance ToQuery DeleteBucketPolicy where
    toQuery DeleteBucketPolicy{..} = mconcat
        [ "policy"
        ]

instance ToHeaders DeleteBucketPolicy

instance ToBody DeleteBucketPolicy

data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteBucketPolicy where
    type Sv DeleteBucketPolicy = S3
    type Rs DeleteBucketPolicy = DeleteBucketPolicyResponse

    request = delete
    response _ = nullaryResponse DeleteBucketPolicyResponse
