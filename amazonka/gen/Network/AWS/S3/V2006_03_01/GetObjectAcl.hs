{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.S3.V2006_03_01.GetObjectAcl where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default GetObjectAcl request.
getObjectAcl :: BucketName -- ^ '_goarBucket'
             -> ObjectKey -- ^ '_goarKey'
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

instance ToPath GetObjectAcl where
    toPath GetObjectAcl{..} = mconcat
        [ "/"
        , toBS _goarBucket
        , "/"
        , toBS _goarKey
        ]

instance ToQuery GetObjectAcl

instance ToHeaders GetObjectAcl

instance ToBody GetObjectAcl

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3

    request  = get
    response = xmlResponse

data instance Rs GetObjectAcl = GetObjectAclResponse
    { _goaoGrants :: [Grant]
      -- ^ A list of grants.
    , _goaoOwner :: Maybe Owner
    } deriving (Show, Generic)

instance FromXML (Rs GetObjectAcl) where
    fromXMLOptions = xmlOptions
