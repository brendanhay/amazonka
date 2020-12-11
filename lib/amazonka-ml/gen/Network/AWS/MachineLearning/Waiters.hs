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
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.MachineLearning.DescribeMLModels' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkMLModelAvailable :: Wait.Wait DescribeMLModels
mkMLModelAvailable =
  Wait.Wait
    { Wait._waitName = "MLModelAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETED"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dmlmsrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. mlmStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dmlmsrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. mlmStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeBatchPredictions' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkBatchPredictionAvailable :: Wait.Wait DescribeBatchPredictions
mkBatchPredictionAvailable =
  Wait.Wait
    { Wait._waitName = "BatchPredictionAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETED"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dbpsrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. bpStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dbpsrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. bpStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeDataSources' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDataSourceAvailable :: Wait.Wait DescribeDataSources
mkDataSourceAvailable =
  Wait.Wait
    { Wait._waitName = "DataSourceAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETED"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (ddssrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddssrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.MachineLearning.DescribeEvaluations' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkEvaluationAvailable :: Wait.Wait DescribeEvaluations
mkEvaluationAvailable =
  Wait.Wait
    { Wait._waitName = "EvaluationAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETED"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (desrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (desrsResults Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }
