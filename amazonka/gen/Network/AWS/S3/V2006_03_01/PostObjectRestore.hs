{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PostObjectRestore
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Restores an archived copy of an object back into Amazon S3.
module Network.AWS.S3.V2006_03_01.PostObjectRestore where

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

-- | Smart constructor utilising default fields to
-- specify the minimum viable PostObjectRestore request.
postObjectRestore :: BucketName -- ^ 'rorBucket'
                  -> ObjectKey -- ^ 'rorKey'
                  -> RestoreRequest -- ^ 'rorRestoreRequest'
                  -> PostObjectRestore
postObjectRestore p1 p2 p3 = PostObjectRestore
    { rorBucket = p1
    , rorKey = p2
    , rorRestoreRequest = p3
    , rorVersionId = Nothing
    }

data PostObjectRestore = PostObjectRestore
    { rorBucket :: BucketName
    , rorKey :: ObjectKey
    , rorRestoreRequest :: RestoreRequest
    , rorVersionId :: Maybe ObjectVersionId
    } deriving (Eq, Show, Generic)

instance ToPath PostObjectRestore where
    toPath PostObjectRestore{..} = mconcat
        [ "/"
        , toBS rorBucket
        , "/"
        , toBS rorKey
        ]

instance ToQuery PostObjectRestore

instance ToHeaders PostObjectRestore

instance ToBody PostObjectRestore where
    toBody = undefined -- toBody . rorRestoreRequest

instance AWSRequest PostObjectRestore where
    type Sv PostObjectRestore = S3

    request  = post
fromList [("payload",Null),("name",String "PostObjectRestoreResponse"),("shape",Object fromList [("streaming",Bool False),("location",String "body"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",Null),("documentation",Null),("common",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",Null),("documentation",Null),("location_name",Null),("xml_name",Null)]),("location_name",Null),("type",String "Text"),("xml_name",Null)]),("fields",Array (fromList []))]

data instance Rs PostObjectRestore = PostObjectRestoreResponse
    deriving (Eq, Show, Generic)
