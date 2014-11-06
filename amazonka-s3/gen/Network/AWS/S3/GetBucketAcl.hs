{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the access control policy for the bucket.
module Network.AWS.S3.GetBucketAcl
    (
    -- * Request
      GetBucketAcl
    -- ** Request constructor
    , getBucketAcl
    -- ** Request lenses
    , gbarBucket

    -- * Response
    , GetBucketAclOutput
    -- ** Response constructor
    , getBucketAclOutput
    -- ** Response lenses
    , gbaoGrants
    , gbaoOwner
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

newtype GetBucketAcl = GetBucketAcl
    { _gbarBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbarBucket' @::@ 'BucketName'
--
getBucketAcl :: BucketName -- ^ 'gbarBucket'
             -> GetBucketAcl
getBucketAcl p1 = GetBucketAcl
    { _gbarBucket = p1
    }

gbarBucket :: Lens' GetBucketAcl BucketName
gbarBucket = lens _gbarBucket (\s a -> s { _gbarBucket = a })

instance ToPath GetBucketAcl where
    toPath GetBucketAcl{..} = mconcat
        [ "/"
        , toText _gbarBucket
        ]

instance ToQuery GetBucketAcl where
    toQuery = const "acl"

instance ToHeaders GetBucketAcl

instance ToBody GetBucketAcl

data GetBucketAclOutput = GetBucketAclOutput
    { _gbaoGrants :: [Grant]
    , _gbaoOwner  :: Maybe Owner
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest GetBucketAcl where
    type Sv GetBucketAcl = S3
    type Rs GetBucketAcl = GetBucketAclOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "AccessControlList"
        <*> x %| "Owner"
