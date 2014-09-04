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
    , mkGetObjectAclRequest
    -- ** Request lenses
    , goarBucket
    , goarKey
    , goarVersionId

    -- * Response
    , GetObjectAclResponse
    -- ** Response lenses
    , goaoOwner
    , goaoGrants
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectAcl' request.
mkGetObjectAclRequest :: BucketName -- ^ 'goarBucket'
                      -> ObjectKey -- ^ 'goarKey'
                      -> GetObjectAcl
mkGetObjectAclRequest p1 p2 = GetObjectAcl
    { _goarBucket = p1
    , _goarKey = p2
    , _goarVersionId = Nothing
    }
{-# INLINE mkGetObjectAclRequest #-}

data GetObjectAcl = GetObjectAcl
    { _goarBucket :: BucketName
    , _goarKey :: ObjectKey
    , _goarVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Show, Generic)

goarBucket :: Lens' GetObjectAcl (BucketName)
goarBucket = lens _goarBucket (\s a -> s { _goarBucket = a })
{-# INLINE goarBucket #-}

goarKey :: Lens' GetObjectAcl (ObjectKey)
goarKey = lens _goarKey (\s a -> s { _goarKey = a })
{-# INLINE goarKey #-}

-- | VersionId used to reference a specific version of the object.
goarVersionId :: Lens' GetObjectAcl (Maybe ObjectVersionId)
goarVersionId = lens _goarVersionId (\s a -> s { _goarVersionId = a })
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
    { _goaoOwner :: Maybe Owner
    , _goaoGrants :: [Grant]
      -- ^ A list of grants.
    } deriving (Show, Generic)

goaoOwner :: Lens' GetObjectAclResponse (Maybe Owner)
goaoOwner = lens _goaoOwner (\s a -> s { _goaoOwner = a })
{-# INLINE goaoOwner #-}

-- | A list of grants.
goaoGrants :: Lens' GetObjectAclResponse ([Grant])
goaoGrants = lens _goaoGrants (\s a -> s { _goaoGrants = a })
{-# INLINE goaoGrants #-}

instance FromXML GetObjectAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3
    type Rs GetObjectAcl = GetObjectAclResponse

    request = get
    response _ = xmlResponse
