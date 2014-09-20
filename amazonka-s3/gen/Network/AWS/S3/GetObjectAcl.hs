{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObjectAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the access control list (ACL) of an object.
module Network.AWS.S3.GetObjectAcl
    (
    -- * Request
      GetObjectAcl
    -- ** Request constructor
    , getObjectAcl
    -- ** Request lenses
    , goaBucket
    , goaKey
    , goaVersionId

    -- * Response
    , GetObjectAclResponse
    -- ** Response constructor
    , getObjectAclResponse
    -- ** Response lenses
    , goarOwner
    , goarGrants
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data GetObjectAcl = GetObjectAcl
    { _goaBucket :: BucketName
    , _goaKey :: ObjectKey
    , _goaVersionId :: Maybe ObjectVersionId
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectAcl' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Key ::@ @ObjectKey@
--
-- * @VersionId ::@ @Maybe ObjectVersionId@
--
getObjectAcl :: BucketName -- ^ 'goaBucket'
             -> ObjectKey -- ^ 'goaKey'
             -> GetObjectAcl
getObjectAcl p1 p2 = GetObjectAcl
    { _goaBucket = p1
    , _goaKey = p2
    , _goaVersionId = Nothing
    }

goaBucket :: Lens' GetObjectAcl BucketName
goaBucket = lens _goaBucket (\s a -> s { _goaBucket = a })

goaKey :: Lens' GetObjectAcl ObjectKey
goaKey = lens _goaKey (\s a -> s { _goaKey = a })

-- | VersionId used to reference a specific version of the object.
goaVersionId :: Lens' GetObjectAcl (Maybe ObjectVersionId)
goaVersionId = lens _goaVersionId (\s a -> s { _goaVersionId = a })

instance ToPath GetObjectAcl

instance ToQuery GetObjectAcl

instance ToHeaders GetObjectAcl

instance ToBody GetObjectAcl

data GetObjectAclResponse = GetObjectAclResponse
    { _goarOwner :: Maybe Owner
    , _goarGrants :: [Grant]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectAclResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Owner ::@ @Maybe Owner@
--
-- * @Grants ::@ @[Grant]@
--
getObjectAclResponse :: GetObjectAclResponse
getObjectAclResponse = GetObjectAclResponse
    { _goarOwner = Nothing
    , _goarGrants = mempty
    }

goarOwner :: Lens' GetObjectAclResponse (Maybe Owner)
goarOwner = lens _goarOwner (\s a -> s { _goarOwner = a })

-- | A list of grants.
goarGrants :: Lens' GetObjectAclResponse [Grant]
goarGrants = lens _goarGrants (\s a -> s { _goarGrants = a })

instance FromXML GetObjectAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3
    type Rs GetObjectAcl = GetObjectAclResponse

    request = get
    response _ = xmlResponse
