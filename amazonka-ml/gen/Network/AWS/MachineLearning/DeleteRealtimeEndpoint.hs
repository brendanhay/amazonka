{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real time endpoint of an 'MLModel'.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteRealtimeEndpoint.html AWS API Reference> for DeleteRealtimeEndpoint.
module Network.AWS.MachineLearning.DeleteRealtimeEndpoint
    (
    -- * Creating a Request
      deleteRealtimeEndpoint
    , DeleteRealtimeEndpoint
    -- * Request Lenses
    , dreMLModelId

    -- * Destructuring the Response
    , deleteRealtimeEndpointResponse
    , DeleteRealtimeEndpointResponse
    -- * Response Lenses
    , drersRealtimeEndpointInfo
    , drersMLModelId
    , drersStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteRealtimeEndpoint' smart constructor.
newtype DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'
    { _dreMLModelId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteRealtimeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreMLModelId'
deleteRealtimeEndpoint
    :: Text -- ^ 'dreMLModelId'
    -> DeleteRealtimeEndpoint
deleteRealtimeEndpoint pMLModelId_ =
    DeleteRealtimeEndpoint'
    { _dreMLModelId = pMLModelId_
    }

-- | The ID assigned to the 'MLModel' during creation.
dreMLModelId :: Lens' DeleteRealtimeEndpoint Text
dreMLModelId = lens _dreMLModelId (\ s a -> s{_dreMLModelId = a});

instance AWSRequest DeleteRealtimeEndpoint where
        type Rs DeleteRealtimeEndpoint =
             DeleteRealtimeEndpointResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRealtimeEndpointResponse' <$>
                   (x .?> "RealtimeEndpointInfo") <*>
                     (x .?> "MLModelId")
                     <*> (pure (fromEnum s)))

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
          = object
              (catMaybes [Just ("MLModelId" .= _dreMLModelId)])

instance ToPath DeleteRealtimeEndpoint where
        toPath = const "/"

instance ToQuery DeleteRealtimeEndpoint where
        toQuery = const mempty

-- | Represents the output of an DeleteRealtimeEndpoint operation.
--
-- The result contains the 'MLModelId' and the endpoint information for the
-- 'MLModel'.
--
-- /See:/ 'deleteRealtimeEndpointResponse' smart constructor.
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'
    { _drersRealtimeEndpointInfo :: !(Maybe RealtimeEndpointInfo)
    , _drersMLModelId            :: !(Maybe Text)
    , _drersStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteRealtimeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drersRealtimeEndpointInfo'
--
-- * 'drersMLModelId'
--
-- * 'drersStatus'
deleteRealtimeEndpointResponse
    :: Int -- ^ 'drersStatus'
    -> DeleteRealtimeEndpointResponse
deleteRealtimeEndpointResponse pStatus_ =
    DeleteRealtimeEndpointResponse'
    { _drersRealtimeEndpointInfo = Nothing
    , _drersMLModelId = Nothing
    , _drersStatus = pStatus_
    }

-- | The endpoint information of the 'MLModel'
drersRealtimeEndpointInfo :: Lens' DeleteRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
drersRealtimeEndpointInfo = lens _drersRealtimeEndpointInfo (\ s a -> s{_drersRealtimeEndpointInfo = a});

-- | A user-supplied ID that uniquely identifies the 'MLModel'. This value
-- should be identical to the value of the 'MLModelId' in the request.
drersMLModelId :: Lens' DeleteRealtimeEndpointResponse (Maybe Text)
drersMLModelId = lens _drersMLModelId (\ s a -> s{_drersMLModelId = a});

-- | The response status code.
drersStatus :: Lens' DeleteRealtimeEndpointResponse Int
drersStatus = lens _drersStatus (\ s a -> s{_drersStatus = a});
