{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.DescribeEndpoint
import Amazonka.SageMaker.DescribeImage
import Amazonka.SageMaker.DescribeImageVersion
import Amazonka.SageMaker.DescribeNotebookInstance
import Amazonka.SageMaker.DescribeProcessingJob
import Amazonka.SageMaker.DescribeTrainingJob
import Amazonka.SageMaker.DescribeTransformJob
import Amazonka.SageMaker.Lens
import Amazonka.SageMaker.Types

-- | Polls 'Amazonka.SageMaker.DescribeImage' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageUpdated :: Core.Wait DescribeImage
newImageUpdated =
  Core.Wait
    { Core._waitName = "ImageUpdated",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATED"
            Core.AcceptSuccess
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeImage' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageDeleted :: Core.Wait DescribeImage
newImageDeleted =
  Core.Wait
    { Core._waitName = "ImageDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Amazonka.SageMaker.DescribeImageVersion' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageVersionDeleted :: Core.Wait DescribeImageVersion
newImageVersionDeleted =
  Core.Wait
    { Core._waitName = "ImageVersionDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeImageVersionResponse_imageVersionStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Amazonka.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
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

-- | Polls 'Amazonka.SageMaker.DescribeImage' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageCreated :: Core.Wait DescribeImage
newImageCreated =
  Core.Wait
    { Core._waitName = "ImageCreated",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATED"
            Core.AcceptSuccess
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeTransformJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Amazonka.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Amazonka.SageMaker.DescribeProcessingJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Amazonka.SageMaker.DescribeImageVersion' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageVersionCreated :: Core.Wait DescribeImageVersion
newImageVersionCreated =
  Core.Wait
    { Core._waitName = "ImageVersionCreated",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATED"
            Core.AcceptSuccess
            ( describeImageVersionResponse_imageVersionStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeImageVersionResponse_imageVersionStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeTrainingJob' every 120 seconds until a successful state is reached. An error is returned after 180 failed checks.
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

-- | Polls 'Amazonka.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
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
