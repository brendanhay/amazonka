{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteMultipleObjects
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
module Network.AWS.S3.V2006_03_01.DeleteMultipleObjects where

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
-- specify the minimum viable DeleteMultipleObjects request.
deleteMultipleObjects :: Delete -- ^ 'dorDelete'
                      -> BucketName -- ^ 'dorBucket'
                      -> DeleteMultipleObjects
deleteMultipleObjects p1 p2 = DeleteMultipleObjects
    { dorDelete = p1
    , dorBucket = p2
    , dorMFA = Nothing
    }

data DeleteMultipleObjects = DeleteMultipleObjects
    { dorDelete :: Delete
    , dorBucket :: BucketName
    , dorMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    } deriving (Eq, Show, Generic)

instance ToPath DeleteMultipleObjects where
    toPath DeleteMultipleObjects{..} = mconcat
        [ "/"
        , toBS dorBucket
        ]

instance ToQuery DeleteMultipleObjects

instance ToHeaders DeleteMultipleObjects where
    toHeaders DeleteMultipleObjects{..} = concat
        [ "x-amz-mfa" =: dorMFA
        ]

instance ToBody DeleteMultipleObjects where
    toBody = undefined -- toBody . dorDelete

instance AWSRequest DeleteMultipleObjects where
    type Sv DeleteMultipleObjects = S3

    request  = post
    response = response' $

data instance Rs DeleteMultipleObjects = DeleteMultipleObjectsResponse
    { dooDeleted :: [DeletedObject]
    , dooErrors :: [Error]
    } deriving (Eq, Show, Generic)
