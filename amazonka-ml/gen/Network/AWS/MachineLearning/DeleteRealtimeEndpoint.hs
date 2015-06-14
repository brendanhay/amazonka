{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
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

-- | Deletes a real time endpoint of an @MLModel@.
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
    , drerRealtimeEndpointInfo
    , drerMLModelId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.MachineLearning.Types

-- | /See:/ 'deleteRealtimeEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dreMLModelId'
newtype DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'{_dreMLModelId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteRealtimeEndpoint' smart constructor.
deleteRealtimeEndpoint :: Text -> DeleteRealtimeEndpoint
deleteRealtimeEndpoint pMLModelId = DeleteRealtimeEndpoint'{_dreMLModelId = pMLModelId};

-- | The ID assigned to the @MLModel@ during creation.
dreMLModelId :: Lens' DeleteRealtimeEndpoint Text
dreMLModelId = lens _dreMLModelId (\ s a -> s{_dreMLModelId = a});

instance AWSRequest DeleteRealtimeEndpoint where
        type Sv DeleteRealtimeEndpoint = MachineLearning
        type Rs DeleteRealtimeEndpoint =
             DeleteRealtimeEndpointResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRealtimeEndpointResponse' <$>
                   x .?> "RealtimeEndpointInfo" <*> x .:> "MLModelId")

instance ToHeaders DeleteRealtimeEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DeleteRealtimeEndpoint" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRealtimeEndpoint where
        toJSON DeleteRealtimeEndpoint'{..}
          = object ["MLModelId" .= _dreMLModelId]

instance ToPath DeleteRealtimeEndpoint where
        toPath = const "/"

instance ToQuery DeleteRealtimeEndpoint where
        toQuery = const mempty

-- | /See:/ 'deleteRealtimeEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drerRealtimeEndpointInfo'
--
-- * 'drerMLModelId'
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'{_drerRealtimeEndpointInfo :: Maybe RealtimeEndpointInfo, _drerMLModelId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteRealtimeEndpointResponse' smart constructor.
deleteRealtimeEndpointResponse :: Text -> DeleteRealtimeEndpointResponse
deleteRealtimeEndpointResponse pMLModelId = DeleteRealtimeEndpointResponse'{_drerRealtimeEndpointInfo = Nothing, _drerMLModelId = pMLModelId};

-- | The endpoint information of the @MLModel@
drerRealtimeEndpointInfo :: Lens' DeleteRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
drerRealtimeEndpointInfo = lens _drerRealtimeEndpointInfo (\ s a -> s{_drerRealtimeEndpointInfo = a});

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
drerMLModelId :: Lens' DeleteRealtimeEndpointResponse Text
drerMLModelId = lens _drerMLModelId (\ s a -> s{_drerMLModelId = a});
