{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateBatchPrediction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Updates the @BatchPredictionName@ of a @BatchPrediction@.
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
    , ubpBatchPredictionId
    , ubpBatchPredictionName

    -- * Response
    , UpdateBatchPredictionResponse
    -- ** Response constructor
    , updateBatchPredictionResponse
    -- ** Response lenses
    , ubprBatchPredictionId
    , ubprStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateBatchPrediction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubpBatchPredictionId'
--
-- * 'ubpBatchPredictionName'
data UpdateBatchPrediction = UpdateBatchPrediction'
    { _ubpBatchPredictionId   :: !Text
    , _ubpBatchPredictionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBatchPrediction' smart constructor.
updateBatchPrediction :: Text -> Text -> UpdateBatchPrediction
updateBatchPrediction pBatchPredictionId pBatchPredictionName =
    UpdateBatchPrediction'
    { _ubpBatchPredictionId = pBatchPredictionId
    , _ubpBatchPredictionName = pBatchPredictionName
    }

-- | The ID assigned to the @BatchPrediction@ during creation.
ubpBatchPredictionId :: Lens' UpdateBatchPrediction Text
ubpBatchPredictionId = lens _ubpBatchPredictionId (\ s a -> s{_ubpBatchPredictionId = a});

-- | A new user-supplied name or description of the @BatchPrediction@.
ubpBatchPredictionName :: Lens' UpdateBatchPrediction Text
ubpBatchPredictionName = lens _ubpBatchPredictionName (\ s a -> s{_ubpBatchPredictionName = a});

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
              ["BatchPredictionId" .= _ubpBatchPredictionId,
               "BatchPredictionName" .= _ubpBatchPredictionName]

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
-- * 'ubprBatchPredictionId'
--
-- * 'ubprStatus'
data UpdateBatchPredictionResponse = UpdateBatchPredictionResponse'
    { _ubprBatchPredictionId :: !(Maybe Text)
    , _ubprStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateBatchPredictionResponse' smart constructor.
updateBatchPredictionResponse :: Int -> UpdateBatchPredictionResponse
updateBatchPredictionResponse pStatus =
    UpdateBatchPredictionResponse'
    { _ubprBatchPredictionId = Nothing
    , _ubprStatus = pStatus
    }

-- | The ID assigned to the @BatchPrediction@ during creation. This value
-- should be identical to the value of the @BatchPredictionId@ in the
-- request.
ubprBatchPredictionId :: Lens' UpdateBatchPredictionResponse (Maybe Text)
ubprBatchPredictionId = lens _ubprBatchPredictionId (\ s a -> s{_ubprBatchPredictionId = a});

-- | FIXME: Undocumented member.
ubprStatus :: Lens' UpdateBatchPredictionResponse Int
ubprStatus = lens _ubprStatus (\ s a -> s{_ubprStatus = a});
