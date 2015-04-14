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

-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a real time endpoint of an 'MLModel'.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteRealtimeEndpoint.html>
module Network.AWS.MachineLearning.DeleteRealtimeEndpoint
    (
    -- * Request
      DeleteRealtimeEndpoint
    -- ** Request constructor
    , deleteRealtimeEndpoint
    -- ** Request lenses
    , dreMLModelId

    -- * Response
    , DeleteRealtimeEndpointResponse
    -- ** Response constructor
    , deleteRealtimeEndpointResponse
    -- ** Response lenses
    , drerMLModelId
    , drerRealtimeEndpointInfo
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype DeleteRealtimeEndpoint = DeleteRealtimeEndpoint
    { _dreMLModelId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteRealtimeEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dreMLModelId' @::@ 'Text'
--
deleteRealtimeEndpoint :: Text -- ^ 'dreMLModelId'
                       -> DeleteRealtimeEndpoint
deleteRealtimeEndpoint p1 = DeleteRealtimeEndpoint
    { _dreMLModelId = p1
    }

-- | The ID assigned to the 'MLModel' during creation.
dreMLModelId :: Lens' DeleteRealtimeEndpoint Text
dreMLModelId = lens _dreMLModelId (\s a -> s { _dreMLModelId = a })

data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse
    { _drerMLModelId            :: Maybe Text
    , _drerRealtimeEndpointInfo :: Maybe RealtimeEndpointInfo
    } deriving (Eq, Read, Show)

-- | 'DeleteRealtimeEndpointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drerMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'drerRealtimeEndpointInfo' @::@ 'Maybe' 'RealtimeEndpointInfo'
--
deleteRealtimeEndpointResponse :: DeleteRealtimeEndpointResponse
deleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse
    { _drerMLModelId            = Nothing
    , _drerRealtimeEndpointInfo = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'MLModel'. This value should be
-- identical to the value of the 'MLModelId' in the request.
drerMLModelId :: Lens' DeleteRealtimeEndpointResponse (Maybe Text)
drerMLModelId = lens _drerMLModelId (\s a -> s { _drerMLModelId = a })

-- | The endpoint information of the 'MLModel'
drerRealtimeEndpointInfo :: Lens' DeleteRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
drerRealtimeEndpointInfo =
    lens _drerRealtimeEndpointInfo
        (\s a -> s { _drerRealtimeEndpointInfo = a })

instance ToPath DeleteRealtimeEndpoint where
    toPath = const "/"

instance ToQuery DeleteRealtimeEndpoint where
    toQuery = const mempty

instance ToHeaders DeleteRealtimeEndpoint

instance ToJSON DeleteRealtimeEndpoint where
    toJSON DeleteRealtimeEndpoint{..} = object
        [ "MLModelId" .= _dreMLModelId
        ]

instance AWSRequest DeleteRealtimeEndpoint where
    type Sv DeleteRealtimeEndpoint = MachineLearning
    type Rs DeleteRealtimeEndpoint = DeleteRealtimeEndpointResponse

    request  = post "DeleteRealtimeEndpoint"
    response = jsonResponse

instance FromJSON DeleteRealtimeEndpointResponse where
    parseJSON = withObject "DeleteRealtimeEndpointResponse" $ \o -> DeleteRealtimeEndpointResponse
        <$> o .:? "MLModelId"
        <*> o .:? "RealtimeEndpointInfo"
