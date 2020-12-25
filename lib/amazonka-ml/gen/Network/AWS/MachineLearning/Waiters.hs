{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Waiters
  ( -- * MLModelAvailable
    mkMLModelAvailable,

    -- * BatchPredictionAvailable
    mkBatchPredictionAvailable,

    -- * DataSourceAvailable
    mkDataSourceAvailable,

    -- * EvaluationAvailable
    mkEvaluationAvailable,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.DescribeBatchPredictions
import Network.AWS.MachineLearning.DescribeDataSources
import Network.AWS.MachineLearning.DescribeEvaluations
import Network.AWS.MachineLearning.DescribeMLModels
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.MachineLearning.DescribeMLModels' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkMLModelAvailable :: Waiter.Wait DescribeMLModels
mkMLModelAvailable =
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
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeBatchPredictions' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkBatchPredictionAvailable :: Waiter.Wait DescribeBatchPredictions
mkBatchPredictionAvailable =
  Waiter.Wait
    { Waiter._waitName = "BatchPredictionAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeDataSources' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDataSourceAvailable :: Waiter.Wait DescribeDataSources
mkDataSourceAvailable =
  Waiter.Wait
    { Waiter._waitName = "DataSourceAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeEvaluations' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkEvaluationAvailable :: Waiter.Wait DescribeEvaluations
mkEvaluationAvailable =
  Waiter.Wait
    { Waiter._waitName = "EvaluationAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"results" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }
