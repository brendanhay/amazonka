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
    , mkDeleteBucketPolicyRequest
    -- ** Request lenses
    , dbprBucket

    -- * Response
    , DeleteBucketPolicyResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketPolicy' request.
mkDeleteBucketPolicyRequest :: BucketName -- ^ 'dbprBucket'
                            -> DeleteBucketPolicy
mkDeleteBucketPolicyRequest p1 = DeleteBucketPolicy
    { _dbprBucket = p1
    }
{-# INLINE mkDeleteBucketPolicyRequest #-}

newtype DeleteBucketPolicy = DeleteBucketPolicy
    { _dbprBucket :: BucketName
    } deriving (Show, Generic)

dbprBucket :: Lens' DeleteBucketPolicy (BucketName)
dbprBucket = lens _dbprBucket (\s a -> s { _dbprBucket = a })
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
