{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Waiters
  ( -- * NotebookInstanceDeleted
    mkNotebookInstanceDeleted,

    -- * EndpointDeleted
    mkEndpointDeleted,

    -- * EndpointInService
    mkEndpointInService,

    -- * TransformJobCompletedOrStopped
    mkTransformJobCompletedOrStopped,

    -- * NotebookInstanceInService
    mkNotebookInstanceInService,

    -- * ProcessingJobCompletedOrStopped
    mkProcessingJobCompletedOrStopped,

    -- * TrainingJobCompletedOrStopped
    mkTrainingJobCompletedOrStopped,

    -- * NotebookInstanceStopped
    mkNotebookInstanceStopped,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeProcessingJob
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import Network.AWS.SageMaker.Types
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkNotebookInstanceDeleted :: Wait.Wait DescribeNotebookInstance
mkNotebookInstanceDeleted =
  Wait.Wait
    { Wait._waitName = "NotebookInstanceDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchError "ValidationException" Wait.AcceptSuccess,
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dnirsNotebookInstanceStatus Lude.. Lens.to Lude.toText)
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkEndpointDeleted :: Wait.Wait DescribeEndpoint
mkEndpointDeleted =
  Wait.Wait
    { Wait._waitName = "EndpointDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchError "ValidationException" Wait.AcceptSuccess,
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dersEndpointStatus Lude.. Lens.to Lude.toText)
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkEndpointInService :: Wait.Wait DescribeEndpoint
mkEndpointInService =
  Wait.Wait
    { Wait._waitName = "EndpointInService",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "InService"
            Wait.AcceptSuccess
            (dersEndpointStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dersEndpointStatus Lude.. Lens.to Lude.toText),
          Wait.matchError "ValidationException" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTransformJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkTransformJobCompletedOrStopped :: Wait.Wait DescribeTransformJob
mkTransformJobCompletedOrStopped =
  Wait.Wait
    { Wait._waitName = "TransformJobCompletedOrStopped",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Completed"
            Wait.AcceptSuccess
            (dtjrsTransformJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Stopped"
            Wait.AcceptSuccess
            (dtjrsTransformJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dtjrsTransformJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchError "ValidationException" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkNotebookInstanceInService :: Wait.Wait DescribeNotebookInstance
mkNotebookInstanceInService =
  Wait.Wait
    { Wait._waitName = "NotebookInstanceInService",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "InService"
            Wait.AcceptSuccess
            (dnirsNotebookInstanceStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dnirsNotebookInstanceStatus Lude.. Lens.to Lude.toText)
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeProcessingJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkProcessingJobCompletedOrStopped :: Wait.Wait DescribeProcessingJob
mkProcessingJobCompletedOrStopped =
  Wait.Wait
    { Wait._waitName = "ProcessingJobCompletedOrStopped",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Completed"
            Wait.AcceptSuccess
            (dpjrsProcessingJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Stopped"
            Wait.AcceptSuccess
            (dpjrsProcessingJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dpjrsProcessingJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchError "ValidationException" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTrainingJob' every 120 seconds until a successful state is reached. An error is returned after 180 failed checks.
mkTrainingJobCompletedOrStopped :: Wait.Wait DescribeTrainingJob
mkTrainingJobCompletedOrStopped =
  Wait.Wait
    { Wait._waitName = "TrainingJobCompletedOrStopped",
      Wait._waitAttempts = 180,
      Wait._waitDelay = 120,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Completed"
            Wait.AcceptSuccess
            (dtjtrsTrainingJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Stopped"
            Wait.AcceptSuccess
            (dtjtrsTrainingJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dtjtrsTrainingJobStatus Lude.. Lens.to Lude.toText),
          Wait.matchError "ValidationException" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkNotebookInstanceStopped :: Wait.Wait DescribeNotebookInstance
mkNotebookInstanceStopped =
  Wait.Wait
    { Wait._waitName = "NotebookInstanceStopped",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Stopped"
            Wait.AcceptSuccess
            (dnirsNotebookInstanceStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (dnirsNotebookInstanceStatus Lude.. Lens.to Lude.toText)
        ]
    }
