{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.RestoreObject
    (
    -- * Request
      RestoreObject
    -- ** Request alias
    , PostObjectRestore
    -- ** Request constructor
    , mkRestoreObject
    -- ** Request lenses
    , roBucket
    , roKey
    , roVersionId
    , roRestoreRequest

    -- * Response
    , RestoreObjectResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type PostObjectRestore = RestoreObject

data RestoreObject = RestoreObject
    { _roBucket :: BucketName
    , _roKey :: ObjectKey
    , _roVersionId :: Maybe ObjectVersionId
    , _roRestoreRequest :: Maybe RestoreRequest
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RestoreObject' request.
mkRestoreObject :: BucketName -- ^ 'roBucket'
                -> ObjectKey -- ^ 'roKey'
                -> RestoreObject
mkRestoreObject p1 p2 = RestoreObject
    { _roBucket = p1
    , _roKey = p2
    , _roVersionId = Nothing
    , _roRestoreRequest = Nothing
    }
{-# INLINE mkRestoreObject #-}

roBucket :: Lens' RestoreObject BucketName
roBucket = lens _roBucket (\s a -> s { _roBucket = a })
{-# INLINE roBucket #-}

roKey :: Lens' RestoreObject ObjectKey
roKey = lens _roKey (\s a -> s { _roKey = a })
{-# INLINE roKey #-}

roVersionId :: Lens' RestoreObject (Maybe ObjectVersionId)
roVersionId = lens _roVersionId (\s a -> s { _roVersionId = a })
{-# INLINE roVersionId #-}

roRestoreRequest :: Lens' RestoreObject (Maybe RestoreRequest)
roRestoreRequest =
    lens _roRestoreRequest (\s a -> s { _roRestoreRequest = a })
{-# INLINE roRestoreRequest #-}

instance ToPath RestoreObject where
    toPath RestoreObject{..} = mconcat
        [ "/"
        , toBS _roBucket
        , "/"
        , toBS _roKey
        ]

instance ToQuery RestoreObject where
    toQuery RestoreObject{..} = mconcat
        [ "restore&versionId" =? _roVersionId
        ]

instance ToHeaders RestoreObject

instance ToBody RestoreObject where
    toBody = toBody . encodeXML . _roRestoreRequest

data RestoreObjectResponse = RestoreObjectResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RestoreObject where
    type Sv RestoreObject = S3
    type Rs RestoreObject = RestoreObjectResponse

    request = post
    response _ = nullaryResponse RestoreObjectResponse
