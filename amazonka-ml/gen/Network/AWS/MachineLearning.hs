-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Definition of the public APIs exposed by Amazon Machine Learning
module Network.AWS.MachineLearning
    ( module Export
    ) where

import           Network.AWS.MachineLearning.CreateBatchPrediction        as Export
import           Network.AWS.MachineLearning.CreateDataSourceFromRDS      as Export
import           Network.AWS.MachineLearning.CreateDataSourceFromRedshift as Export
import           Network.AWS.MachineLearning.CreateDataSourceFromS        as Export
import           Network.AWS.MachineLearning.CreateEvaluation             as Export
import           Network.AWS.MachineLearning.CreateMLModel                as Export
import           Network.AWS.MachineLearning.CreateRealtimeEndpoint       as Export
import           Network.AWS.MachineLearning.DeleteBatchPrediction        as Export
import           Network.AWS.MachineLearning.DeleteDataSource             as Export
import           Network.AWS.MachineLearning.DeleteEvaluation             as Export
import           Network.AWS.MachineLearning.DeleteMLModel                as Export
import           Network.AWS.MachineLearning.DeleteRealtimeEndpoint       as Export
import           Network.AWS.MachineLearning.DescribeBatchPredictions     as Export
import           Network.AWS.MachineLearning.DescribeDataSources          as Export
import           Network.AWS.MachineLearning.DescribeEvaluations          as Export
import           Network.AWS.MachineLearning.DescribeMLModels             as Export
import           Network.AWS.MachineLearning.GetBatchPrediction           as Export
import           Network.AWS.MachineLearning.GetDataSource                as Export
import           Network.AWS.MachineLearning.GetEvaluation                as Export
import           Network.AWS.MachineLearning.GetMLModel                   as Export
import           Network.AWS.MachineLearning.Predict                      as Export
import           Network.AWS.MachineLearning.Types                        as Export
import           Network.AWS.MachineLearning.UpdateBatchPrediction        as Export
import           Network.AWS.MachineLearning.UpdateDataSource             as Export
import           Network.AWS.MachineLearning.UpdateEvaluation             as Export
import           Network.AWS.MachineLearning.UpdateMLModel                as Export
import           Network.AWS.MachineLearning.Waiters                      as Export
