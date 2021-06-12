{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.DescribeBatchPredictions
import Network.AWS.MachineLearning.DescribeDataSources
import Network.AWS.MachineLearning.DescribeEvaluations
import Network.AWS.MachineLearning.DescribeMLModels
import Network.AWS.MachineLearning.Lens
import Network.AWS.MachineLearning.Types

-- | Polls 'Network.AWS.MachineLearning.DescribeMLModels' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newMLModelAvailable :: Core.Wait DescribeMLModels
newMLModelAvailable =
  Core.Wait
    { Core._waitName = "MLModelAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeMLModelsResponse_results Core.. Lens._Just)
                )
                Core.. mLModel_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeMLModelsResponse_results Core.. Lens._Just)
                )
                Core.. mLModel_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeEvaluations' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEvaluationAvailable :: Core.Wait DescribeEvaluations
newEvaluationAvailable =
  Core.Wait
    { Core._waitName = "EvaluationAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEvaluationsResponse_results
                        Core.. Lens._Just
                    )
                )
                Core.. evaluation_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEvaluationsResponse_results
                        Core.. Lens._Just
                    )
                )
                Core.. evaluation_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeDataSources' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDataSourceAvailable :: Core.Wait DescribeDataSources
newDataSourceAvailable =
  Core.Wait
    { Core._waitName = "DataSourceAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDataSourcesResponse_results
                        Core.. Lens._Just
                    )
                )
                Core.. dataSource_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDataSourcesResponse_results
                        Core.. Lens._Just
                    )
                )
                Core.. dataSource_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeBatchPredictions' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newBatchPredictionAvailable :: Core.Wait DescribeBatchPredictions
newBatchPredictionAvailable =
  Core.Wait
    { Core._waitName =
        "BatchPredictionAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBatchPredictionsResponse_results
                        Core.. Lens._Just
                    )
                )
                Core.. batchPrediction_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBatchPredictionsResponse_results
                        Core.. Lens._Just
                    )
                )
                Core.. batchPrediction_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }
