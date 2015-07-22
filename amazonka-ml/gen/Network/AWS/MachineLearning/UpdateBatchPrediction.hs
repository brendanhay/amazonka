{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateBatchPrediction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the @BatchPredictionName@ of a @BatchPrediction@.
--
-- You can use the GetBatchPrediction operation to view the contents of the
-- updated data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateBatchPrediction.html>
module Network.AWS.MachineLearning.UpdateBatchPrediction
    (
    -- * Request
      UpdateBatchPrediction
    -- ** Request constructor
    , updateBatchPrediction
    -- ** Request lenses
    , ubprqBatchPredictionId
    , ubprqBatchPredictionName

    -- * Response
    , UpdateBatchPredictionResponse
    -- ** Response constructor
    , updateBatchPredictionResponse
    -- ** Response lenses
    , ubprsBatchPredictionId
    , ubprsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateBatchPrediction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubprqBatchPredictionId'
--
-- * 'ubprqBatchPredictionName'
data UpdateBatchPrediction = UpdateBatchPrediction'
    { _ubprqBatchPredictionId   :: !Text
    , _ubprqBatchPredictionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBatchPrediction' smart constructor.
updateBatchPrediction :: Text -> Text -> UpdateBatchPrediction
updateBatchPrediction pBatchPredictionId pBatchPredictionName =
    UpdateBatchPrediction'
    { _ubprqBatchPredictionId = pBatchPredictionId
    , _ubprqBatchPredictionName = pBatchPredictionName
    }

-- | The ID assigned to the @BatchPrediction@ during creation.
ubprqBatchPredictionId :: Lens' UpdateBatchPrediction Text
ubprqBatchPredictionId = lens _ubprqBatchPredictionId (\ s a -> s{_ubprqBatchPredictionId = a});

-- | A new user-supplied name or description of the @BatchPrediction@.
ubprqBatchPredictionName :: Lens' UpdateBatchPrediction Text
ubprqBatchPredictionName = lens _ubprqBatchPredictionName (\ s a -> s{_ubprqBatchPredictionName = a});

instance AWSRequest UpdateBatchPrediction where
        type Sv UpdateBatchPrediction = MachineLearning
        type Rs UpdateBatchPrediction =
             UpdateBatchPredictionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateBatchPredictionResponse' <$>
                   (x .?> "BatchPredictionId") <*> (pure (fromEnum s)))

instance ToHeaders UpdateBatchPrediction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.UpdateBatchPrediction" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBatchPrediction where
        toJSON UpdateBatchPrediction'{..}
          = object
              ["BatchPredictionId" .= _ubprqBatchPredictionId,
               "BatchPredictionName" .= _ubprqBatchPredictionName]

instance ToPath UpdateBatchPrediction where
        toPath = const "/"

instance ToQuery UpdateBatchPrediction where
        toQuery = const mempty

-- | Represents the output of an UpdateBatchPrediction operation.
--
-- You can see the updated content by using the GetBatchPrediction
-- operation.
--
-- /See:/ 'updateBatchPredictionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubprsBatchPredictionId'
--
-- * 'ubprsStatus'
data UpdateBatchPredictionResponse = UpdateBatchPredictionResponse'
    { _ubprsBatchPredictionId :: !(Maybe Text)
    , _ubprsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBatchPredictionResponse' smart constructor.
updateBatchPredictionResponse :: Int -> UpdateBatchPredictionResponse
updateBatchPredictionResponse pStatus =
    UpdateBatchPredictionResponse'
    { _ubprsBatchPredictionId = Nothing
    , _ubprsStatus = pStatus
    }

-- | The ID assigned to the @BatchPrediction@ during creation. This value
-- should be identical to the value of the @BatchPredictionId@ in the
-- request.
ubprsBatchPredictionId :: Lens' UpdateBatchPredictionResponse (Maybe Text)
ubprsBatchPredictionId = lens _ubprsBatchPredictionId (\ s a -> s{_ubprsBatchPredictionId = a});

-- | FIXME: Undocumented member.
ubprsStatus :: Lens' UpdateBatchPredictionResponse Int
ubprsStatus = lens _ubprsStatus (\ s a -> s{_ubprsStatus = a});
