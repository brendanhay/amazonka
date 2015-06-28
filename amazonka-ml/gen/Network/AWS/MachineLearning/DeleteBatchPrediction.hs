{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.DeleteBatchPrediction
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

-- | Assigns the DELETED status to a @BatchPrediction@, rendering it
-- unusable.
--
-- After using the @DeleteBatchPrediction@ operation, you can use the
-- GetBatchPrediction operation to verify that the status of the
-- @BatchPrediction@ changed to DELETED.
--
-- Caution
--
-- The result of the @DeleteBatchPrediction@ operation is irreversible.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteBatchPrediction.html>
module Network.AWS.MachineLearning.DeleteBatchPrediction
    (
    -- * Request
      DeleteBatchPrediction
    -- ** Request constructor
    , deleteBatchPrediction
    -- ** Request lenses
    , dbpBatchPredictionId

    -- * Response
    , DeleteBatchPredictionResponse
    -- ** Response constructor
    , deleteBatchPredictionResponse
    -- ** Response lenses
    , dbprBatchPredictionId
    , dbprStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteBatchPrediction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpBatchPredictionId'
newtype DeleteBatchPrediction = DeleteBatchPrediction'
    { _dbpBatchPredictionId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteBatchPrediction' smart constructor.
deleteBatchPrediction :: Text -> DeleteBatchPrediction
deleteBatchPrediction pBatchPredictionId =
    DeleteBatchPrediction'
    { _dbpBatchPredictionId = pBatchPredictionId
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@.
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
                   (x .?> "BatchPredictionId") <*> (pure s))

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
              ["BatchPredictionId" .= _dbpBatchPredictionId]

instance ToPath DeleteBatchPrediction where
        toPath = const "/"

instance ToQuery DeleteBatchPrediction where
        toQuery = const mempty

-- | Represents the output of a DeleteBatchPrediction operation.
--
-- You can use the GetBatchPrediction operation and check the value of the
-- @Status@ parameter to see whether a @BatchPrediction@ is marked as
-- @DELETED@.
--
-- /See:/ 'deleteBatchPredictionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbprBatchPredictionId'
--
-- * 'dbprStatus'
data DeleteBatchPredictionResponse = DeleteBatchPredictionResponse'
    { _dbprBatchPredictionId :: !(Maybe Text)
    , _dbprStatus            :: !Status
    } deriving (Eq,Show)

-- | 'DeleteBatchPredictionResponse' smart constructor.
deleteBatchPredictionResponse :: Status -> DeleteBatchPredictionResponse
deleteBatchPredictionResponse pStatus =
    DeleteBatchPredictionResponse'
    { _dbprBatchPredictionId = Nothing
    , _dbprStatus = pStatus
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@. This
-- value should be identical to the value of the @BatchPredictionID@ in the
-- request.
dbprBatchPredictionId :: Lens' DeleteBatchPredictionResponse (Maybe Text)
dbprBatchPredictionId = lens _dbprBatchPredictionId (\ s a -> s{_dbprBatchPredictionId = a});

-- | FIXME: Undocumented member.
dbprStatus :: Lens' DeleteBatchPredictionResponse Status
dbprStatus = lens _dbprStatus (\ s a -> s{_dbprStatus = a});
