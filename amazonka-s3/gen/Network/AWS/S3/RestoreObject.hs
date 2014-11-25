{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.RestoreObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Restores an archived copy of an object back into Amazon S3
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RestoreObject.html>
module Network.AWS.S3.RestoreObject
    (
    -- * Request
      RestoreObject
    -- ** Request constructor
    , restoreObject
    -- ** Request lenses
    , roBucket
    , roKey
    , roRestoreRequest
    , roVersionId

    -- * Response
    , RestoreObjectResponse
    -- ** Response constructor
    , restoreObjectResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data RestoreObject = RestoreObject
    { _roBucket         :: Text
    , _roKey            :: Text
    , _roRestoreRequest :: Maybe RestoreRequest
    , _roVersionId      :: Maybe Text
    } deriving (Eq, Show)

-- | 'RestoreObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'roBucket' @::@ 'Text'
--
-- * 'roKey' @::@ 'Text'
--
-- * 'roRestoreRequest' @::@ 'Maybe' 'RestoreRequest'
--
-- * 'roVersionId' @::@ 'Maybe' 'Text'
--
restoreObject :: Text -- ^ 'roBucket'
              -> Text -- ^ 'roKey'
              -> RestoreObject
restoreObject p1 p2 = RestoreObject
    { _roBucket         = p1
    , _roKey            = p2
    , _roVersionId      = Nothing
    , _roRestoreRequest = Nothing
    }

roBucket :: Lens' RestoreObject Text
roBucket = lens _roBucket (\s a -> s { _roBucket = a })

roKey :: Lens' RestoreObject Text
roKey = lens _roKey (\s a -> s { _roKey = a })

roRestoreRequest :: Lens' RestoreObject (Maybe RestoreRequest)
roRestoreRequest = lens _roRestoreRequest (\s a -> s { _roRestoreRequest = a })

roVersionId :: Lens' RestoreObject (Maybe Text)
roVersionId = lens _roVersionId (\s a -> s { _roVersionId = a })

data RestoreObjectResponse = RestoreObjectResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RestoreObjectResponse' constructor.
restoreObjectResponse :: RestoreObjectResponse
restoreObjectResponse = RestoreObjectResponse

instance ToPath RestoreObject where
    toPath RestoreObject{..} = mconcat
        [ "/"
        , toText _roBucket
        , "/"
        , toText _roKey
        ]

instance ToQuery RestoreObject where
    toQuery RestoreObject{..} = mconcat
        [ "restore"
        , "versionId" =? _roVersionId
        ]

instance ToHeaders RestoreObject

instance ToXMLRoot RestoreObject where
    toXMLRoot RestoreObject{..} = namespaced ns "RestoreObject"
        [ "RestoreRequest" =@ _roRestoreRequest
        ]

instance ToXML RestoreObject

instance AWSRequest RestoreObject where
    type Sv RestoreObject = S3
    type Rs RestoreObject = RestoreObjectResponse

    request  = post
    response = nullResponse RestoreObjectResponse
