{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn't a null version, Amazon S3 does not remove any objects.
module Network.AWS.S3.V2006_03_01.DeleteObject where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Default DeleteObject request.
deleteObject :: BucketName -- ^ 'dorBucket'
             -> ObjectKey -- ^ 'dorKey'
             -> DeleteObject
deleteObject p1 p2 = DeleteObject
    { dorBucket = p1
    , dorKey = p2
    , dorMFA = Nothing
    , dorVersionId = Nothing
    }

data DeleteObject = DeleteObject
    { dorBucket :: BucketName
    , dorKey :: ObjectKey
    , dorMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    , dorVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Eq, Show, Generic)

instance ToPath DeleteObject where
    toPath DeleteObject{..} = mconcat
        [ "/"
        , toBS dorBucket
        , "/"
        , toBS dorKey
        ]

instance ToQuery DeleteObject

instance ToHeaders DeleteObject where
    toHeaders DeleteObject{..} = concat
        [ "x-amz-mfa" =: dorMFA
        ]

instance ToBody DeleteObject

instance AWSRequest DeleteObject where
    type Sv DeleteObject = S3

    request  = delete
    response = headerResponse $ \hs ->
        pure DeleteObjectResponse
            <*> hs ~:? "x-amz-delete-marker" hs
            <*> hs ~:? "x-amz-version-id" hs

data instance Rs DeleteObject = DeleteObjectResponse
    { dooDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the versioned object that was permanently
      -- deleted was (true) or was not (false) a delete marker.
    , dooVersionId :: Maybe ObjectVersionId
      -- ^ Returns the version ID of the delete marker created as a result
      -- of the DELETE operation.
    } deriving (Eq, Show, Generic)
