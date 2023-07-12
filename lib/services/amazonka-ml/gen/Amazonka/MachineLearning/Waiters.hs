{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.DescribeBatchPredictions
import Amazonka.MachineLearning.DescribeDataSources
import Amazonka.MachineLearning.DescribeEvaluations
import Amazonka.MachineLearning.DescribeMLModels
import Amazonka.MachineLearning.Lens
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.MachineLearning.DescribeBatchPredictions' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newBatchPredictionAvailable :: Core.Wait DescribeBatchPredictions
newBatchPredictionAvailable =
  Core.Wait
    { Core.name = "BatchPredictionAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MachineLearning.DescribeDataSources' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDataSourceAvailable :: Core.Wait DescribeDataSources
newDataSourceAvailable =
  Core.Wait
    { Core.name = "DataSourceAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MachineLearning.DescribeEvaluations' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEvaluationAvailable :: Core.Wait DescribeEvaluations
newEvaluationAvailable =
  Core.Wait
    { Core.name = "EvaluationAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MachineLearning.DescribeMLModels' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newMLModelAvailable :: Core.Wait DescribeMLModels
newMLModelAvailable =
  Core.Wait
    { Core.name = "MLModelAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
