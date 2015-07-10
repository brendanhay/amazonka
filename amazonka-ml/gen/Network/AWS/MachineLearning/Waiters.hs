{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Waiters where

import           Network.AWS.MachineLearning.DescribeBatchPredictions
import           Network.AWS.MachineLearning.DescribeDataSources
import           Network.AWS.MachineLearning.DescribeEvaluations
import           Network.AWS.MachineLearning.DescribeMLModels
import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

mLModelAvailable :: Wait DescribeMLModels
mLModelAvailable =
    Wait
    { _waitName = "MLModelAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "COMPLETED"
                             AcceptSuccess
                             (folding (concatOf descResults) .
                              mlmStatus . _Just . to toText)
                       , matchAny
                             "FAILED"
                             AcceptFailure
                             (folding (concatOf descResults) .
                              mlmStatus . _Just . to toText)]
    }

batchPredictionAvailable :: Wait DescribeBatchPredictions
batchPredictionAvailable =
    Wait
    { _waitName = "BatchPredictionAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "COMPLETED"
                             AcceptSuccess
                             (folding (concatOf desResults) .
                              bpStatus . _Just . to toText)
                       , matchAny
                             "FAILED"
                             AcceptFailure
                             (folding (concatOf desResults) .
                              bpStatus . _Just . to toText)]
    }

dataSourceAvailable :: Wait DescribeDataSources
dataSourceAvailable =
    Wait
    { _waitName = "DataSourceAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "COMPLETED"
                             AcceptSuccess
                             (folding (concatOf dResults) .
                              dsStatus . _Just . to toText)
                       , matchAny
                             "FAILED"
                             AcceptFailure
                             (folding (concatOf dResults) .
                              dsStatus . _Just . to toText)]
    }

evaluationAvailable :: Wait DescribeEvaluations
evaluationAvailable =
    Wait
    { _waitName = "EvaluationAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "COMPLETED"
                             AcceptSuccess
                             (folding (concatOf der1Results) .
                              evaStatus . _Just . to toText)
                       , matchAny
                             "FAILED"
                             AcceptFailure
                             (folding (concatOf der1Results) .
                              evaStatus . _Just . to toText)]
    }
