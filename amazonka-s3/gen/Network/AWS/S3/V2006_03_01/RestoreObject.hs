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
    , mkRestoreObjectRequest
    -- ** Request lenses
    , rorBucket
    , rorKey
    , rorVersionId
    , rorRestoreRequest

    -- * Response
    , RestoreObjectResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type PostObjectRestore = RestoreObject

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RestoreObject' request.
mkRestoreObjectRequest :: BucketName -- ^ 'rorBucket'
                       -> ObjectKey -- ^ 'rorKey'
                       -> RestoreObject
mkRestoreObjectRequest p1 p2 = RestoreObject
    { _rorBucket = p1
    , _rorKey = p2
    , _rorVersionId = Nothing
    , _rorRestoreRequest = Nothing
    }
{-# INLINE mkRestoreObjectRequest #-}

data RestoreObject = RestoreObject
    { _rorBucket :: BucketName
    , _rorKey :: ObjectKey
    , _rorVersionId :: Maybe ObjectVersionId
    , _rorRestoreRequest :: Maybe RestoreRequest
    } deriving (Show, Generic)

rorBucket :: Lens' RestoreObject (BucketName)
rorBucket = lens _rorBucket (\s a -> s { _rorBucket = a })
{-# INLINE rorBucket #-}

rorKey :: Lens' RestoreObject (ObjectKey)
rorKey = lens _rorKey (\s a -> s { _rorKey = a })
{-# INLINE rorKey #-}

rorVersionId :: Lens' RestoreObject (Maybe ObjectVersionId)
rorVersionId = lens _rorVersionId (\s a -> s { _rorVersionId = a })
{-# INLINE rorVersionId #-}

rorRestoreRequest :: Lens' RestoreObject (Maybe RestoreRequest)
rorRestoreRequest = lens _rorRestoreRequest (\s a -> s { _rorRestoreRequest = a })
{-# INLINE rorRestoreRequest #-}

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

instance ToBody RestoreObject

data RestoreObjectResponse = RestoreObjectResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RestoreObject where
    type Sv RestoreObject = S3
    type Rs RestoreObject = RestoreObjectResponse

    request = post
    response _ = nullaryResponse RestoreObjectResponse
