{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , drerStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteRealtimeEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dreMLModelId'
newtype DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'
    { _dreMLModelId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRealtimeEndpoint' smart constructor.
deleteRealtimeEndpoint :: Text -> DeleteRealtimeEndpoint
deleteRealtimeEndpoint pMLModelId =
    DeleteRealtimeEndpoint'
    { _dreMLModelId = pMLModelId
    }

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
          = object ["MLModelId" .= _dreMLModelId]

instance ToPath DeleteRealtimeEndpoint where
        toPath = const "/"

instance ToQuery DeleteRealtimeEndpoint where
        toQuery = const mempty

-- | Represents the output of an DeleteRealtimeEndpoint operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the
-- @MLModel@.
--
-- /See:/ 'deleteRealtimeEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drerRealtimeEndpointInfo'
--
-- * 'drerMLModelId'
--
-- * 'drerStatus'
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'
    { _drerRealtimeEndpointInfo :: !(Maybe RealtimeEndpointInfo)
    , _drerMLModelId            :: !(Maybe Text)
    , _drerStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRealtimeEndpointResponse' smart constructor.
deleteRealtimeEndpointResponse :: Int -> DeleteRealtimeEndpointResponse
deleteRealtimeEndpointResponse pStatus =
    DeleteRealtimeEndpointResponse'
    { _drerRealtimeEndpointInfo = Nothing
    , _drerMLModelId = Nothing
    , _drerStatus = pStatus
    }

-- | The endpoint information of the @MLModel@
drerRealtimeEndpointInfo :: Lens' DeleteRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
drerRealtimeEndpointInfo = lens _drerRealtimeEndpointInfo (\ s a -> s{_drerRealtimeEndpointInfo = a});

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelId@ in the request.
drerMLModelId :: Lens' DeleteRealtimeEndpointResponse (Maybe Text)
drerMLModelId = lens _drerMLModelId (\ s a -> s{_drerMLModelId = a});

-- | FIXME: Undocumented member.
drerStatus :: Lens' DeleteRealtimeEndpointResponse Int
drerStatus = lens _drerStatus (\ s a -> s{_drerStatus = a});
