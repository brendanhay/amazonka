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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.DescribeBatchPredictions
import Network.AWS.MachineLearning.DescribeDataSources
import Network.AWS.MachineLearning.DescribeEvaluations
import Network.AWS.MachineLearning.DescribeMLModels
import Network.AWS.MachineLearning.Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.MachineLearning.DescribeMLModels' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newMLModelAvailable :: Waiter.Wait DescribeMLModels
newMLModelAvailable =
  Waiter.Wait
    { Waiter._waitName = "MLModelAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeMLModelsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. mLModel_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeMLModelsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. mLModel_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeEvaluations' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEvaluationAvailable :: Waiter.Wait DescribeEvaluations
newEvaluationAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "EvaluationAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEvaluationsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. evaluation_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEvaluationsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. evaluation_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeDataSources' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDataSourceAvailable :: Waiter.Wait DescribeDataSources
newDataSourceAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "DataSourceAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDataSourcesResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dataSource_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDataSourcesResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dataSource_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeBatchPredictions' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newBatchPredictionAvailable :: Waiter.Wait DescribeBatchPredictions
newBatchPredictionAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "BatchPredictionAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBatchPredictionsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. batchPrediction_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBatchPredictionsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. batchPrediction_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
