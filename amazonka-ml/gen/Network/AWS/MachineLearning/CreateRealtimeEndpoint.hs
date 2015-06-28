{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.CreateRealtimeEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a real-time endpoint for the @MLModel@. The endpoint contains
-- the URI of the @MLModel@; that is, the location to send real-time
-- prediction requests for the specified @MLModel@.
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
    , crerRealtimeEndpointInfo
    , crerMLModelId
    , crerStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRealtimeEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creMLModelId'
newtype CreateRealtimeEndpoint = CreateRealtimeEndpoint'
    { _creMLModelId :: Text
    } deriving (Eq,Read,Show)

-- | 'CreateRealtimeEndpoint' smart constructor.
createRealtimeEndpoint :: Text -> CreateRealtimeEndpoint
createRealtimeEndpoint pMLModelId =
    CreateRealtimeEndpoint'
    { _creMLModelId = pMLModelId
    }

-- | The ID assigned to the @MLModel@ during creation.
creMLModelId :: Lens' CreateRealtimeEndpoint Text
creMLModelId = lens _creMLModelId (\ s a -> s{_creMLModelId = a});

instance AWSRequest CreateRealtimeEndpoint where
        type Sv CreateRealtimeEndpoint = MachineLearning
        type Rs CreateRealtimeEndpoint =
             CreateRealtimeEndpointResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateRealtimeEndpointResponse' <$>
                   (x .?> "RealtimeEndpointInfo") <*>
                     (x .?> "MLModelId")
                     <*> (pure s))

instance ToHeaders CreateRealtimeEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateRealtimeEndpoint" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRealtimeEndpoint where
        toJSON CreateRealtimeEndpoint'{..}
          = object ["MLModelId" .= _creMLModelId]

instance ToPath CreateRealtimeEndpoint where
        toPath = const "/"

instance ToQuery CreateRealtimeEndpoint where
        toQuery = const mempty

-- | Represents the output of an CreateRealtimeEndpoint operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the
-- @MLModel@.
--
-- The endpoint information includes the URI of the @MLModel@; that is, the
-- location to send online prediction requests for the specified @MLModel@.
--
-- /See:/ 'createRealtimeEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crerRealtimeEndpointInfo'
--
-- * 'crerMLModelId'
--
-- * 'crerStatus'
data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse'
    { _crerRealtimeEndpointInfo :: !(Maybe RealtimeEndpointInfo)
    , _crerMLModelId            :: !(Maybe Text)
    , _crerStatus               :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateRealtimeEndpointResponse' smart constructor.
createRealtimeEndpointResponse :: Status -> CreateRealtimeEndpointResponse
createRealtimeEndpointResponse pStatus =
    CreateRealtimeEndpointResponse'
    { _crerRealtimeEndpointInfo = Nothing
    , _crerMLModelId = Nothing
    , _crerStatus = pStatus
    }

-- | The endpoint information of the @MLModel@
crerRealtimeEndpointInfo :: Lens' CreateRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
crerRealtimeEndpointInfo = lens _crerRealtimeEndpointInfo (\ s a -> s{_crerRealtimeEndpointInfo = a});

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
crerMLModelId :: Lens' CreateRealtimeEndpointResponse (Maybe Text)
crerMLModelId = lens _crerMLModelId (\ s a -> s{_crerMLModelId = a});

-- | FIXME: Undocumented member.
crerStatus :: Lens' CreateRealtimeEndpointResponse Status
crerStatus = lens _crerStatus (\ s a -> s{_crerStatus = a});
