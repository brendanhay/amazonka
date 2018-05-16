{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Waiters where

import Network.AWS.Lens
import Network.AWS.MachineLearning.DescribeBatchPredictions
import Network.AWS.MachineLearning.DescribeDataSources
import Network.AWS.MachineLearning.DescribeEvaluations
import Network.AWS.MachineLearning.DescribeMLModels
import Network.AWS.MachineLearning.Types
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.MachineLearning.DescribeMLModels' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mLModelAvailable :: Wait DescribeMLModels
mLModelAvailable =
  Wait
    { _waitName = "MLModelAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "COMPLETED"
            AcceptSuccess
            (folding (concatOf dmlmsrsResults) . mlmStatus . _Just . to toTextCI)
        , matchAny
            "FAILED"
            AcceptFailure
            (folding (concatOf dmlmsrsResults) . mlmStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.MachineLearning.DescribeBatchPredictions' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
batchPredictionAvailable :: Wait DescribeBatchPredictions
batchPredictionAvailable =
  Wait
    { _waitName = "BatchPredictionAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "COMPLETED"
            AcceptSuccess
            (folding (concatOf dbpsrsResults) . bpStatus . _Just . to toTextCI)
        , matchAny
            "FAILED"
            AcceptFailure
            (folding (concatOf dbpsrsResults) . bpStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.MachineLearning.DescribeDataSources' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
dataSourceAvailable :: Wait DescribeDataSources
dataSourceAvailable =
  Wait
    { _waitName = "DataSourceAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "COMPLETED"
            AcceptSuccess
            (folding (concatOf ddssrsResults) . dsStatus . _Just . to toTextCI)
        , matchAny
            "FAILED"
            AcceptFailure
            (folding (concatOf ddssrsResults) . dsStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.MachineLearning.DescribeEvaluations' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
evaluationAvailable :: Wait DescribeEvaluations
evaluationAvailable =
  Wait
    { _waitName = "EvaluationAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "COMPLETED"
            AcceptSuccess
            (folding (concatOf desrsResults) . eStatus . _Just . to toTextCI)
        , matchAny
            "FAILED"
            AcceptFailure
            (folding (concatOf desrsResults) . eStatus . _Just . to toTextCI)
        ]
    }

