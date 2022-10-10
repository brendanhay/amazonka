{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MachineLearning.DescribeBatchPredictions
import Amazonka.MachineLearning.DescribeDataSources
import Amazonka.MachineLearning.DescribeEvaluations
import Amazonka.MachineLearning.DescribeMLModels
import Amazonka.MachineLearning.Lens
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.MachineLearning.DescribeEvaluations' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. evaluation_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEvaluationsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. evaluation_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MachineLearning.DescribeMLModels' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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
                    ( describeMLModelsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. mLModel_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeMLModelsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. mLModel_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MachineLearning.DescribeDataSources' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dataSource_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDataSourcesResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dataSource_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MachineLearning.DescribeBatchPredictions' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. batchPrediction_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeBatchPredictionsResponse_results
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. batchPrediction_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
