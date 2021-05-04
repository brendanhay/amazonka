{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Waiters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeProcessingJob
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import Network.AWS.SageMaker.Lens
import Network.AWS.SageMaker.Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newNotebookInstanceStopped :: Waiter.Wait DescribeNotebookInstance
newNotebookInstanceStopped =
  Waiter.Wait
    { Waiter._waitName =
        "NotebookInstanceStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEndpointDeleted :: Waiter.Wait DescribeEndpoint
newEndpointDeleted =
  Waiter.Wait
    { Waiter._waitName = "EndpointDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "ValidationException"
            Waiter.AcceptSuccess,
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newNotebookInstanceDeleted :: Waiter.Wait DescribeNotebookInstance
newNotebookInstanceDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "NotebookInstanceDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "ValidationException"
            Waiter.AcceptSuccess,
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newNotebookInstanceInService :: Waiter.Wait DescribeNotebookInstance
newNotebookInstanceInService =
  Waiter.Wait
    { Waiter._waitName =
        "NotebookInstanceInService",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "InService"
            Waiter.AcceptSuccess
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newEndpointInService :: Waiter.Wait DescribeEndpoint
newEndpointInService =
  Waiter.Wait
    { Waiter._waitName = "EndpointInService",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "InService"
            Waiter.AcceptSuccess
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationException"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTrainingJob' every 120 seconds until a successful state is reached. An error is returned after 180 failed checks.
newTrainingJobCompletedOrStopped :: Waiter.Wait DescribeTrainingJob
newTrainingJobCompletedOrStopped =
  Waiter.Wait
    { Waiter._waitName =
        "TrainingJobCompletedOrStopped",
      Waiter._waitAttempts = 180,
      Waiter._waitDelay = 120,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationException"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeProcessingJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newProcessingJobCompletedOrStopped :: Waiter.Wait DescribeProcessingJob
newProcessingJobCompletedOrStopped =
  Waiter.Wait
    { Waiter._waitName =
        "ProcessingJobCompletedOrStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationException"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTransformJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newTransformJobCompletedOrStopped :: Waiter.Wait DescribeTransformJob
newTransformJobCompletedOrStopped =
  Waiter.Wait
    { Waiter._waitName =
        "TransformJobCompletedOrStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Stopped"
            Waiter.AcceptSuccess
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationException"
            Waiter.AcceptFailure
        ]
    }
