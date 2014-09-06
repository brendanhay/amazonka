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
    , mkDeleteBucketPolicy
    -- ** Request lenses
    , dbpBucket

    -- * Response
    , DeleteBucketPolicyResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype DeleteBucketPolicy = DeleteBucketPolicy
    { _dbpBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketPolicy' request.
mkDeleteBucketPolicy :: BucketName -- ^ 'dbpBucket'
                     -> DeleteBucketPolicy
mkDeleteBucketPolicy p1 = DeleteBucketPolicy
    { _dbpBucket = p1
    }
{-# INLINE mkDeleteBucketPolicy #-}

dbpBucket :: Lens' DeleteBucketPolicy BucketName
dbpBucket = lens _dbpBucket (\s a -> s { _dbpBucket = a })
{-# INLINE dbpBucket #-}

instance ToPath DeleteBucketPolicy where
    toPath DeleteBucketPolicy{..} = mconcat
        [ "/"
        , toBS _dbpBucket
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
