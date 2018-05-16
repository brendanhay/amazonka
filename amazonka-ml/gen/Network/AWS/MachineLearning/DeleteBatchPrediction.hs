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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @BatchPrediction@ , rendering it unusable.
--
--
-- After using the @DeleteBatchPrediction@ operation, you can use the 'GetBatchPrediction' operation to verify that the status of the @BatchPrediction@ changed to DELETED.
--
-- __Caution:__ The result of the @DeleteBatchPrediction@ operation is irreversible.
--
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
    , dbprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBatchPrediction' smart constructor.
newtype DeleteBatchPrediction = DeleteBatchPrediction'
  { _dbpBatchPredictionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBatchPrediction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbpBatchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ .
deleteBatchPrediction
    :: Text -- ^ 'dbpBatchPredictionId'
    -> DeleteBatchPrediction
deleteBatchPrediction pBatchPredictionId_ =
  DeleteBatchPrediction' {_dbpBatchPredictionId = pBatchPredictionId_}


-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ .
dbpBatchPredictionId :: Lens' DeleteBatchPrediction Text
dbpBatchPredictionId = lens _dbpBatchPredictionId (\ s a -> s{_dbpBatchPredictionId = a})

instance AWSRequest DeleteBatchPrediction where
        type Rs DeleteBatchPrediction =
             DeleteBatchPredictionResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBatchPredictionResponse' <$>
                   (x .?> "BatchPredictionId") <*> (pure (fromEnum s)))

instance Hashable DeleteBatchPrediction where

instance NFData DeleteBatchPrediction where

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

-- | Represents the output of a @DeleteBatchPrediction@ operation.
--
--
-- You can use the @GetBatchPrediction@ operation and check the value of the @Status@ parameter to see whether a @BatchPrediction@ is marked as @DELETED@ .
--
--
-- /See:/ 'deleteBatchPredictionResponse' smart constructor.
data DeleteBatchPredictionResponse = DeleteBatchPredictionResponse'
  { _dbprsBatchPredictionId :: !(Maybe Text)
  , _dbprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBatchPredictionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbprsBatchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value should be identical to the value of the @BatchPredictionID@ in the request.
--
-- * 'dbprsResponseStatus' - -- | The response status code.
deleteBatchPredictionResponse
    :: Int -- ^ 'dbprsResponseStatus'
    -> DeleteBatchPredictionResponse
deleteBatchPredictionResponse pResponseStatus_ =
  DeleteBatchPredictionResponse'
    {_dbprsBatchPredictionId = Nothing, _dbprsResponseStatus = pResponseStatus_}


-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value should be identical to the value of the @BatchPredictionID@ in the request.
dbprsBatchPredictionId :: Lens' DeleteBatchPredictionResponse (Maybe Text)
dbprsBatchPredictionId = lens _dbprsBatchPredictionId (\ s a -> s{_dbprsBatchPredictionId = a})

-- | -- | The response status code.
dbprsResponseStatus :: Lens' DeleteBatchPredictionResponse Int
dbprsResponseStatus = lens _dbprsResponseStatus (\ s a -> s{_dbprsResponseStatus = a})

instance NFData DeleteBatchPredictionResponse where
