{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type PostObjectRestore = RestoreObject

-- | Minimum specification for a 'RestoreObject' request.
restoreObject :: BucketName -- ^ '_rorBucket'
              -> ObjectKey -- ^ '_rorKey'
              -> RestoreObject
restoreObject p1 p2 = RestoreObject
    { _rorBucket = p1
    , _rorKey = p2
    , _rorRestoreRequest = Nothing
    , _rorVersionId = Nothing
    }

data RestoreObject = RestoreObject
    { _rorBucket :: BucketName
    , _rorKey :: ObjectKey
    , _rorRestoreRequest :: Maybe RestoreRequest
    , _rorVersionId :: Maybe ObjectVersionId
    } deriving (Show, Generic)

makeLenses ''RestoreObject

instance ToPath RestoreObject where
    toPath RestoreObject{..} = mconcat
        [ "/"
        , toBS _rorBucket
        , "/"
        , toBS _rorKey
        ]

instance ToQuery RestoreObject where
    toQuery RestoreObject{..} = mconcat
        [ "restore&versionId" =? _rorVersionId
        ]

instance ToHeaders RestoreObject

instance ToBody RestoreObject where
    toBody = toBody . encodeXML . _rorRestoreRequest

data RestoreObjectResponse = RestoreObjectResponse
    deriving (Eq, Show, Generic)

makeLenses ''RestoreObjectResponse

instance AWSRequest RestoreObject where
    type Sv RestoreObject = S3
    type Rs RestoreObject = RestoreObjectResponse

    request = post
    response _ _ = return (Right RestoreObjectResponse)
