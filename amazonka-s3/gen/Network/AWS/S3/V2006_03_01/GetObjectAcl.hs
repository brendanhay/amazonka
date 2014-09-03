{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetObjectAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the access control list (ACL) of an object.
module Network.AWS.S3.V2006_03_01.GetObjectAcl
    (
    -- * Request
      GetObjectAcl
    -- ** Request constructor
    , getObjectAcl
    -- ** Request lenses
    , goarBucket
    , goarKey
    , goarVersionId

    -- * Response
    , GetObjectAclResponse
    -- ** Response lenses
    , goaoGrants
    , goaoOwner
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetObjectAcl' request.
getObjectAcl :: BucketName -- ^ 'goarBucket'
             -> ObjectKey -- ^ 'goarKey'
             -> GetObjectAcl
getObjectAcl p1 p2 = GetObjectAcl
    { _goarBucket = p1
    , _goarKey = p2
    , _goarVersionId = Nothing
    }

data GetObjectAcl = GetObjectAcl
    { _goarBucket :: BucketName
    , _goarKey :: ObjectKey
    , _goarVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Show, Generic)

goarBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> GetObjectAcl
    -> f GetObjectAcl
goarBucket f x =
    (\y -> x { _goarBucket = y })
       <$> f (_goarBucket x)
{-# INLINE goarBucket #-}

goarKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> GetObjectAcl
    -> f GetObjectAcl
goarKey f x =
    (\y -> x { _goarKey = y })
       <$> f (_goarKey x)
{-# INLINE goarKey #-}

-- | VersionId used to reference a specific version of the object.
goarVersionId
    :: Functor f
    => (Maybe ObjectVersionId
    -> f (Maybe ObjectVersionId))
    -> GetObjectAcl
    -> f GetObjectAcl
goarVersionId f x =
    (\y -> x { _goarVersionId = y })
       <$> f (_goarVersionId x)
{-# INLINE goarVersionId #-}

instance ToPath GetObjectAcl where
    toPath GetObjectAcl{..} = mconcat
        [ "/"
        , toBS _goarBucket
        , "/"
        , toBS _goarKey
        ]

instance ToQuery GetObjectAcl where
    toQuery GetObjectAcl{..} = mconcat
        [ "acl&versionId" =? _goarVersionId
        ]

instance ToHeaders GetObjectAcl

instance ToBody GetObjectAcl

data GetObjectAclResponse = GetObjectAclResponse
    { _goaoGrants :: [Grant]
      -- ^ A list of grants.
    , _goaoOwner :: Maybe Owner
    } deriving (Show, Generic)

-- | A list of grants.
goaoGrants
    :: Functor f
    => ([Grant]
    -> f ([Grant]))
    -> GetObjectAclResponse
    -> f GetObjectAclResponse
goaoGrants f x =
    (\y -> x { _goaoGrants = y })
       <$> f (_goaoGrants x)
{-# INLINE goaoGrants #-}

goaoOwner
    :: Functor f
    => (Maybe Owner
    -> f (Maybe Owner))
    -> GetObjectAclResponse
    -> f GetObjectAclResponse
goaoOwner f x =
    (\y -> x { _goaoOwner = y })
       <$> f (_goaoOwner x)
{-# INLINE goaoOwner #-}

instance FromXML GetObjectAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3
    type Rs GetObjectAcl = GetObjectAclResponse

    request = get
    response _ = xmlResponse
