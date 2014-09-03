{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the access control policy for the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketAcl
    (
    -- * Request
      GetBucketAcl
    -- ** Request constructor
    , getBucketAcl
    -- ** Request lenses
    , gbarBucket

    -- * Response
    , GetBucketAclResponse
    -- ** Response lenses
    , gbaoGrants
    , gbaoOwner
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetBucketAcl' request.
getBucketAcl :: BucketName -- ^ 'gbarBucket'
             -> GetBucketAcl
getBucketAcl p1 = GetBucketAcl
    { _gbarBucket = p1
    }

data GetBucketAcl = GetBucketAcl
    { _gbarBucket :: BucketName
    } deriving (Show, Generic)

gbarBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> GetBucketAcl
    -> f GetBucketAcl
gbarBucket f x =
    (\y -> x { _gbarBucket = y })
       <$> f (_gbarBucket x)
{-# INLINE gbarBucket #-}

instance ToPath GetBucketAcl where
    toPath GetBucketAcl{..} = mconcat
        [ "/"
        , toBS _gbarBucket
        ]

instance ToQuery GetBucketAcl where
    toQuery GetBucketAcl{..} = mconcat
        [ "acl"
        ]

instance ToHeaders GetBucketAcl

instance ToBody GetBucketAcl

data GetBucketAclResponse = GetBucketAclResponse
    { _gbaoGrants :: [Grant]
      -- ^ A list of grants.
    , _gbaoOwner :: Maybe Owner
    } deriving (Show, Generic)

-- | A list of grants.
gbaoGrants
    :: Functor f
    => ([Grant]
    -> f ([Grant]))
    -> GetBucketAclResponse
    -> f GetBucketAclResponse
gbaoGrants f x =
    (\y -> x { _gbaoGrants = y })
       <$> f (_gbaoGrants x)
{-# INLINE gbaoGrants #-}

gbaoOwner
    :: Functor f
    => (Maybe Owner
    -> f (Maybe Owner))
    -> GetBucketAclResponse
    -> f GetBucketAclResponse
gbaoOwner f x =
    (\y -> x { _gbaoOwner = y })
       <$> f (_gbaoOwner x)
{-# INLINE gbaoOwner #-}

instance FromXML GetBucketAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketAcl where
    type Sv GetBucketAcl = S3
    type Rs GetBucketAcl = GetBucketAclResponse

    request = get
    response _ = xmlResponse
