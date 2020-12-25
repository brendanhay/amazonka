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
import qualified Network.AWS.Prelude as Core
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeProcessingJob
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import qualified Network.AWS.SageMaker.Types as Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkNotebookInstanceDeleted :: Waiter.Wait DescribeNotebookInstance
mkNotebookInstanceDeleted =
  Waiter.Wait
    { Waiter._waitName = "NotebookInstanceDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchError "ValidationException" Waiter.AcceptSuccess,
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"notebookInstanceStatus")
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkEndpointDeleted :: Waiter.Wait DescribeEndpoint
mkEndpointDeleted =
  Waiter.Wait
    { Waiter._waitName = "EndpointDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchError "ValidationException" Waiter.AcceptSuccess,
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"endpointStatus")
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkEndpointInService :: Waiter.Wait DescribeEndpoint
mkEndpointInService =
  Waiter.Wait
    { Waiter._waitName = "EndpointInService",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "InService"
            Waiter.AcceptSuccess
            (Lens.field @"endpointStatus"),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"endpointStatus"),
          Waiter.matchError "ValidationException" Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTransformJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkTransformJobCompletedOrStopped :: Waiter.Wait DescribeTransformJob
mkTransformJobCompletedOrStopped =
  Waiter.Wait
    { Waiter._waitName = "TransformJobCompletedOrStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            (Lens.field @"transformJobStatus"),
          Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            (Lens.field @"transformJobStatus"),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"transformJobStatus"),
          Waiter.matchError "ValidationException" Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkNotebookInstanceInService :: Waiter.Wait DescribeNotebookInstance
mkNotebookInstanceInService =
  Waiter.Wait
    { Waiter._waitName = "NotebookInstanceInService",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "InService"
            Waiter.AcceptSuccess
            (Lens.field @"notebookInstanceStatus"),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"notebookInstanceStatus")
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeProcessingJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkProcessingJobCompletedOrStopped :: Waiter.Wait DescribeProcessingJob
mkProcessingJobCompletedOrStopped =
  Waiter.Wait
    { Waiter._waitName = "ProcessingJobCompletedOrStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            (Lens.field @"processingJobStatus"),
          Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            (Lens.field @"processingJobStatus"),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"processingJobStatus"),
          Waiter.matchError "ValidationException" Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTrainingJob' every 120 seconds until a successful state is reached. An error is returned after 180 failed checks.
mkTrainingJobCompletedOrStopped :: Waiter.Wait DescribeTrainingJob
mkTrainingJobCompletedOrStopped =
  Waiter.Wait
    { Waiter._waitName = "TrainingJobCompletedOrStopped",
      Waiter._waitAttempts = 180,
      Waiter._waitDelay = 120,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            (Lens.field @"trainingJobStatus"),
          Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            (Lens.field @"trainingJobStatus"),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"trainingJobStatus"),
          Waiter.matchError "ValidationException" Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkNotebookInstanceStopped :: Waiter.Wait DescribeNotebookInstance
mkNotebookInstanceStopped =
  Waiter.Wait
    { Waiter._waitName = "NotebookInstanceStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            (Lens.field @"notebookInstanceStatus"),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"notebookInstanceStatus")
        ]
    }
