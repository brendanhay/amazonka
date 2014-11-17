{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the policy from the bucket.
--
-- <DeleteBucketPolicy.html>
module Network.AWS.S3.DeleteBucketPolicy
    (
    -- * Request
      DeleteBucketPolicy
    -- ** Request constructor
    , deleteBucketPolicy
    -- ** Request lenses
    , dbpBucket

    -- * Response
    , DeleteBucketPolicyResponse
    -- ** Response constructor
    , deleteBucketPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype DeleteBucketPolicy = DeleteBucketPolicy
    { _dbpBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteBucketPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpBucket' @::@ 'Text'
--
deleteBucketPolicy :: Text -- ^ 'dbpBucket'
                   -> DeleteBucketPolicy
deleteBucketPolicy p1 = DeleteBucketPolicy
    { _dbpBucket = p1
    }

dbpBucket :: Lens' DeleteBucketPolicy Text
dbpBucket = lens _dbpBucket (\s a -> s { _dbpBucket = a })

data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteBucketPolicyResponse' constructor.
deleteBucketPolicyResponse :: DeleteBucketPolicyResponse
deleteBucketPolicyResponse = DeleteBucketPolicyResponse

instance AWSRequest DeleteBucketPolicy where
    type Sv DeleteBucketPolicy = S3
    type Rs DeleteBucketPolicy = DeleteBucketPolicyResponse

    request  = delete
    response = nullResponse DeleteBucketPolicyResponse

instance ToPath DeleteBucketPolicy where
    toPath DeleteBucketPolicy{..} = mconcat
        [ "/"
        , toText _dbpBucket
        ]

instance ToHeaders DeleteBucketPolicy

instance ToQuery DeleteBucketPolicy where
    toQuery = const "policy"

instance ToXML DeleteBucketPolicy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteBucketPolicy"
