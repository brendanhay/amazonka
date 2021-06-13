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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeProcessingJob
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import Network.AWS.SageMaker.Lens
import Network.AWS.SageMaker.Types

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newNotebookInstanceStopped :: Core.Wait DescribeNotebookInstance
newNotebookInstanceStopped =
  Core.Wait
    { Core._waitName =
        "NotebookInstanceStopped",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEndpointDeleted :: Core.Wait DescribeEndpoint
newEndpointDeleted =
  Core.Wait
    { Core._waitName = "EndpointDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchError
            "ValidationException"
            Core.AcceptSuccess,
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newNotebookInstanceDeleted :: Core.Wait DescribeNotebookInstance
newNotebookInstanceDeleted =
  Core.Wait
    { Core._waitName =
        "NotebookInstanceDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchError
            "ValidationException"
            Core.AcceptSuccess,
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newNotebookInstanceInService :: Core.Wait DescribeNotebookInstance
newNotebookInstanceInService =
  Core.Wait
    { Core._waitName =
        "NotebookInstanceInService",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "InService"
            Core.AcceptSuccess
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newEndpointInService :: Core.Wait DescribeEndpoint
newEndpointInService =
  Core.Wait
    { Core._waitName = "EndpointInService",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "InService"
            Core.AcceptSuccess
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTrainingJob' every 120 seconds until a successful state is reached. An error is returned after 180 failed checks.
newTrainingJobCompletedOrStopped :: Core.Wait DescribeTrainingJob
newTrainingJobCompletedOrStopped =
  Core.Wait
    { Core._waitName =
        "TrainingJobCompletedOrStopped",
      Core._waitAttempts = 180,
      Core._waitDelay = 120,
      Core._waitAcceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeProcessingJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newProcessingJobCompletedOrStopped :: Core.Wait DescribeProcessingJob
newProcessingJobCompletedOrStopped =
  Core.Wait
    { Core._waitName =
        "ProcessingJobCompletedOrStopped",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.SageMaker.DescribeTransformJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newTransformJobCompletedOrStopped :: Core.Wait DescribeTransformJob
newTransformJobCompletedOrStopped =
  Core.Wait
    { Core._waitName =
        "TransformJobCompletedOrStopped",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }
