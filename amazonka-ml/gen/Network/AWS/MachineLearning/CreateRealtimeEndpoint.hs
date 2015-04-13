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

-- Module      : Network.AWS.MachineLearning.CreateRealtimeEndpoint
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

-- | Creates a real-time endpoint for the 'MLModel'. The endpoint contains the URI
-- of the 'MLModel'; that is, the location to send real-time prediction requests
-- for the specified 'MLModel'.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateRealtimeEndpoint.html>
module Network.AWS.MachineLearning.CreateRealtimeEndpoint
    (
    -- * Request
      CreateRealtimeEndpoint
    -- ** Request constructor
    , createRealtimeEndpoint
    -- ** Request lenses
    , creMLModelId

    -- * Response
    , CreateRealtimeEndpointResponse
    -- ** Response constructor
    , createRealtimeEndpointResponse
    -- ** Response lenses
    , crerMLModelId
    , crerRealtimeEndpointInfo
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype CreateRealtimeEndpoint = CreateRealtimeEndpoint
    { _creMLModelId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'CreateRealtimeEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creMLModelId' @::@ 'Text'
--
createRealtimeEndpoint :: Text -- ^ 'creMLModelId'
                       -> CreateRealtimeEndpoint
createRealtimeEndpoint p1 = CreateRealtimeEndpoint
    { _creMLModelId = p1
    }

-- | The ID assigned to the 'MLModel' during creation.
creMLModelId :: Lens' CreateRealtimeEndpoint Text
creMLModelId = lens _creMLModelId (\s a -> s { _creMLModelId = a })

data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse
    { _crerMLModelId            :: Maybe Text
    , _crerRealtimeEndpointInfo :: Maybe RealtimeEndpointInfo
    } deriving (Eq, Read, Show)

-- | 'CreateRealtimeEndpointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crerMLModelId' @::@ 'Maybe' 'Text'
--
-- * 'crerRealtimeEndpointInfo' @::@ 'Maybe' 'RealtimeEndpointInfo'
--
createRealtimeEndpointResponse :: CreateRealtimeEndpointResponse
createRealtimeEndpointResponse = CreateRealtimeEndpointResponse
    { _crerMLModelId            = Nothing
    , _crerRealtimeEndpointInfo = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'MLModel'. This value should be
-- identical to the value of the 'MLModelId' in the request.
crerMLModelId :: Lens' CreateRealtimeEndpointResponse (Maybe Text)
crerMLModelId = lens _crerMLModelId (\s a -> s { _crerMLModelId = a })

-- | The endpoint information of the 'MLModel'
crerRealtimeEndpointInfo :: Lens' CreateRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
crerRealtimeEndpointInfo =
    lens _crerRealtimeEndpointInfo
        (\s a -> s { _crerRealtimeEndpointInfo = a })

instance ToPath CreateRealtimeEndpoint where
    toPath = const "/"

instance ToQuery CreateRealtimeEndpoint where
    toQuery = const mempty

instance ToHeaders CreateRealtimeEndpoint

instance ToJSON CreateRealtimeEndpoint where
    toJSON CreateRealtimeEndpoint{..} = object
        [ "MLModelId" .= _creMLModelId
        ]

instance AWSRequest CreateRealtimeEndpoint where
    type Sv CreateRealtimeEndpoint = MachineLearning
    type Rs CreateRealtimeEndpoint = CreateRealtimeEndpointResponse

    request  = post "CreateRealtimeEndpoint"
    response = jsonResponse

instance FromJSON CreateRealtimeEndpointResponse where
    parseJSON = withObject "CreateRealtimeEndpointResponse" $ \o -> CreateRealtimeEndpointResponse
        <$> o .:? "MLModelId"
        <*> o .:? "RealtimeEndpointInfo"
