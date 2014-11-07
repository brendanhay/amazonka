{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.RestoreObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Restores an archived copy of an object back into Amazon S3.
module Network.AWS.S3.RestoreObject
    (
    -- * Request
      RestoreObject
    -- ** Request constructor
    , restoreObject
    -- ** Request lenses
    , rorBucket
    , rorKey
    , rorRestoreRequest
    , rorVersionId

    -- * Response
    , RestoreObjectResponse
    -- ** Response constructor
    , restoreObjectResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data RestoreObject = RestoreObject
    { _rorBucket         :: Text
    , _rorKey            :: Text
    , _rorRestoreRequest :: Maybe RestoreRequest
    , _rorVersionId      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'RestoreObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rorBucket' @::@ 'Text'
--
-- * 'rorKey' @::@ 'Text'
--
-- * 'rorRestoreRequest' @::@ 'Maybe' 'RestoreRequest'
--
-- * 'rorVersionId' @::@ 'Maybe' 'Text'
--
restoreObject :: Text -- ^ 'rorBucket'
              -> Text -- ^ 'rorKey'
              -> RestoreObject
restoreObject p1 p2 = RestoreObject
    { _rorBucket         = p1
    , _rorKey            = p2
    , _rorVersionId      = Nothing
    , _rorRestoreRequest = Nothing
    }

rorBucket :: Lens' RestoreObject Text
rorBucket = lens _rorBucket (\s a -> s { _rorBucket = a })

rorKey :: Lens' RestoreObject Text
rorKey = lens _rorKey (\s a -> s { _rorKey = a })

rorRestoreRequest :: Lens' RestoreObject (Maybe RestoreRequest)
rorRestoreRequest =
    lens _rorRestoreRequest (\s a -> s { _rorRestoreRequest = a })

rorVersionId :: Lens' RestoreObject (Maybe Text)
rorVersionId = lens _rorVersionId (\s a -> s { _rorVersionId = a })

instance ToPath RestoreObject where
    toPath RestoreObject{..} = mconcat
        [ "/"
        , toText _rorBucket
        , "/"
        , toText _rorKey
        ]

instance ToQuery RestoreObject where
    toQuery RestoreObject{..} = mconcat
        [ "restore"
        , "versionId" =? _rorVersionId
        ]

instance ToHeaders RestoreObject

instance ToBody RestoreObject where
    toBody = toBody . encodeXML . _rorRestoreRequest

data RestoreObjectResponse = RestoreObjectResponse
-- | 'RestoreObjectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
restoreObjectResponse :: RestoreObjectResponse
restoreObjectResponse = RestoreObjectResponse

instance AWSRequest RestoreObject where
    type Sv RestoreObject = S3
    type Rs RestoreObject = RestoreObjectResponse

    request  = post'
    response = const (nullaryResponse RestoreObjectResponse)
