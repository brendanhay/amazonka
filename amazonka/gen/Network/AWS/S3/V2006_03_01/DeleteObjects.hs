{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation enables you to delete multiple objects from a bucket using a
-- single HTTP request. You may specify up to 1000 keys.
module Network.AWS.S3.V2006_03_01.DeleteObjects where

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

type DeleteMultipleObjects = DeleteObjects
type DeleteMultipleObjectsResponse = Rs DeleteObjects

-- | Default DeleteObjects request.
deleteObjects :: Delete -- ^ 'dorDelete'
              -> BucketName -- ^ 'dorBucket'
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { dorDelete = p1
    , dorBucket = p2
    , dorMFA = Nothing
    }

data DeleteObjects = DeleteObjects
    { dorDelete :: Delete
    , dorBucket :: BucketName
    , dorMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    } deriving (Show, Generic)

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toBS dorBucket
        ]

instance ToQuery DeleteObjects

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = concat
        [ "x-amz-mfa" =: dorMFA
        ]

instance ToBody DeleteObjects where
    toBody = undefined -- toBody . dorDelete

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3

    request  = post

data instance Rs DeleteObjects = DeleteObjectsResponse
    { dooDeleted :: [DeletedObject]
    , dooErrors :: [Error]
    } deriving (Show, Generic)
