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
    , mkGetObjectAcl
    -- ** Request lenses
    , goaBucket
    , goaKey
    , goaVersionId

    -- * Response
    , GetObjectAclResponse
    -- ** Response lenses
    , goarsOwner
    , goarsGrants
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data GetObjectAcl = GetObjectAcl
    { _goaBucket :: BucketName
    , _goaKey :: ObjectKey
    , _goaVersionId :: Maybe ObjectVersionId
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectAcl' request.
mkGetObjectAcl :: BucketName -- ^ 'goaBucket'
               -> ObjectKey -- ^ 'goaKey'
               -> GetObjectAcl
mkGetObjectAcl p1 p2 = GetObjectAcl
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

instance ToPath GetObjectAcl where
    toPath GetObjectAcl{..} = mconcat
        [ "/"
        , toBS _goaBucket
        , "/"
        , toBS _goaKey
        ]

instance ToQuery GetObjectAcl where
    toQuery GetObjectAcl{..} = mconcat
        [ "acl&versionId" =? _goaVersionId
        ]

instance ToHeaders GetObjectAcl

instance ToBody GetObjectAcl

data GetObjectAclResponse = GetObjectAclResponse
    { _goarsOwner :: Maybe Owner
    , _goarsGrants :: [Grant]
    } deriving (Show, Generic)

goarsOwner :: Lens' GetObjectAclResponse (Maybe Owner)
goarsOwner = lens _goarsOwner (\s a -> s { _goarsOwner = a })

-- | A list of grants.
goarsGrants :: Lens' GetObjectAclResponse [Grant]
goarsGrants = lens _goarsGrants (\s a -> s { _goarsGrants = a })

instance FromXML GetObjectAclResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3
    type Rs GetObjectAcl = GetObjectAclResponse

    request = get
    response _ = xmlResponse
