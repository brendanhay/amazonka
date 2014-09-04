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
{-# INLINE getObjectAcl #-}

data GetObjectAcl = GetObjectAcl
    { _goarBucket :: BucketName
    , _goarKey :: ObjectKey
    , _goarVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Show, Generic)

goarBucket :: Lens' GetObjectAcl (BucketName)
goarBucket f x =
    f (_goarBucket x)
        <&> \y -> x { _goarBucket = y }
{-# INLINE goarBucket #-}

goarKey :: Lens' GetObjectAcl (ObjectKey)
goarKey f x =
    f (_goarKey x)
        <&> \y -> x { _goarKey = y }
{-# INLINE goarKey #-}

-- | VersionId used to reference a specific version of the object.
goarVersionId :: Lens' GetObjectAcl (Maybe ObjectVersionId)
goarVersionId f x =
    f (_goarVersionId x)
        <&> \y -> x { _goarVersionId = y }
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
goaoGrants :: Lens' GetObjectAclResponse ([Grant])
goaoGrants f x =
    f (_goaoGrants x)
        <&> \y -> x { _goaoGrants = y }
{-# INLINE goaoGrants #-}

goaoOwner :: Lens' GetObjectAclResponse (Maybe Owner)
goaoOwner f x =
    f (_goaoOwner x)
        <&> \y -> x { _goaoOwner = y }
{-# INLINE goaoOwner #-}

instance FromXML GetObjectAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3
    type Rs GetObjectAcl = GetObjectAclResponse

    request = get
    response _ = xmlResponse
