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
-- Module      : Network.AWS.MachineLearning.DeleteBatchPrediction
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a 'BatchPrediction', rendering it
-- unusable.
--
-- After using the 'DeleteBatchPrediction' operation, you can use the
-- GetBatchPrediction operation to verify that the status of the
-- 'BatchPrediction' changed to DELETED.
--
-- Caution
--
-- The result of the 'DeleteBatchPrediction' operation is irreversible.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteBatchPrediction.html AWS API Reference> for DeleteBatchPrediction.
module Network.AWS.MachineLearning.DeleteBatchPrediction
    (
    -- * Creating a Request
      deleteBatchPrediction
    , DeleteBatchPrediction
    -- * Request Lenses
    , dbpBatchPredictionId

    -- * Destructuring the Response
    , deleteBatchPredictionResponse
    , DeleteBatchPredictionResponse
    -- * Response Lenses
    , dbprsBatchPredictionId
    , dbprsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteBatchPrediction' smart constructor.
newtype DeleteBatchPrediction = DeleteBatchPrediction'
    { _dbpBatchPredictionId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBatchPrediction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbpBatchPredictionId'
deleteBatchPrediction
    :: Text -- ^ 'dbpBatchPredictionId'
    -> DeleteBatchPrediction
deleteBatchPrediction pBatchPredictionId_ =
    DeleteBatchPrediction'
    { _dbpBatchPredictionId = pBatchPredictionId_
    }

-- | A user-supplied ID that uniquely identifies the 'BatchPrediction'.
dbpBatchPredictionId :: Lens' DeleteBatchPrediction Text
dbpBatchPredictionId = lens _dbpBatchPredictionId (\ s a -> s{_dbpBatchPredictionId = a});

instance AWSRequest DeleteBatchPrediction where
        type Sv DeleteBatchPrediction = MachineLearning
        type Rs DeleteBatchPrediction =
             DeleteBatchPredictionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBatchPredictionResponse' <$>
                   (x .?> "BatchPredictionId") <*> (pure (fromEnum s)))

instance ToHeaders DeleteBatchPrediction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DeleteBatchPrediction" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteBatchPrediction where
        toJSON DeleteBatchPrediction'{..}
          = object
              (catMaybes
                 [Just
                    ("BatchPredictionId" .= _dbpBatchPredictionId)])

instance ToPath DeleteBatchPrediction where
        toPath = const "/"

instance ToQuery DeleteBatchPrediction where
        toQuery = const mempty

-- | Represents the output of a DeleteBatchPrediction operation.
--
-- You can use the GetBatchPrediction operation and check the value of the
-- 'Status' parameter to see whether a 'BatchPrediction' is marked as
-- 'DELETED'.
--
-- /See:/ 'deleteBatchPredictionResponse' smart constructor.
data DeleteBatchPredictionResponse = DeleteBatchPredictionResponse'
    { _dbprsBatchPredictionId :: !(Maybe Text)
    , _dbprsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteBatchPredictionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbprsBatchPredictionId'
--
-- * 'dbprsStatus'
deleteBatchPredictionResponse
    :: Int -- ^ 'dbprsStatus'
    -> DeleteBatchPredictionResponse
deleteBatchPredictionResponse pStatus_ =
    DeleteBatchPredictionResponse'
    { _dbprsBatchPredictionId = Nothing
    , _dbprsStatus = pStatus_
    }

-- | A user-supplied ID that uniquely identifies the 'BatchPrediction'. This
-- value should be identical to the value of the 'BatchPredictionID' in the
-- request.
dbprsBatchPredictionId :: Lens' DeleteBatchPredictionResponse (Maybe Text)
dbprsBatchPredictionId = lens _dbprsBatchPredictionId (\ s a -> s{_dbprsBatchPredictionId = a});

-- | The response status code.
dbprsStatus :: Lens' DeleteBatchPredictionResponse Int
dbprsStatus = lens _dbprsStatus (\ s a -> s{_dbprsStatus = a});
