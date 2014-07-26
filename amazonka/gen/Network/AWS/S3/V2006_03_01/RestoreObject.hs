{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.RestoreObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Restores an archived copy of an object back into Amazon S3.
module Network.AWS.S3.V2006_03_01.RestoreObject where

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
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

type PostObjectRestore = RestoreObject
True

-- | Default RestoreObject request.
restoreObject :: BucketName -- ^ '_rorBucket'
              -> ObjectKey -- ^ '_rorKey'
              -> RestoreRequest -- ^ '_rorRestoreRequest'
              -> RestoreObject
restoreObject p1 p2 p3 = RestoreObject
    { _rorBucket = p1
    , _rorKey = p2
    , _rorRestoreRequest = p3
    , _rorVersionId = Nothing
    }

data RestoreObject = RestoreObject
    { _rorBucket :: BucketName
    , _rorKey :: ObjectKey
    , _rorRestoreRequest :: RestoreRequest
    , _rorVersionId :: Maybe ObjectVersionId
    } deriving (Generic)

instance ToPath RestoreObject where
    toPath RestoreObject{..} = mconcat
        [ "/"
        , toBS _rorBucket
        , "/"
        , toBS _rorKey
        ]

instance ToQuery RestoreObject

instance ToHeaders RestoreObject

instance ToBody RestoreObject where
    toBody = toBody . encodeXML . _rorRestoreRequest

instance AWSRequest RestoreObject where
    type Sv RestoreObject = S3
    type Rs RestoreObject = RestoreObjectResponse

    request = post

    response _ _ = return (Right RestoreObjectResponse)

data RestoreObjectResponse = RestoreObjectResponse
    deriving (Eq, Show, Generic)
