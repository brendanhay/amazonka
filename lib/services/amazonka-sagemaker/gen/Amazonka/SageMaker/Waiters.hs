{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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

-- | Polls 'Amazonka.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newEndpointInService :: Core.Wait DescribeEndpoint
newEndpointInService =
  Core.Wait
    { Core.name = "EndpointInService",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "InService"
            Core.AcceptSuccess
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Data.toTextCI
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
    { Core.name = "NotebookInstanceStopped",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEndpointDeleted :: Core.Wait DescribeEndpoint
newEndpointDeleted =
  Core.Wait
    { Core.name = "EndpointDeleted",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchError
            "ValidationException"
            Core.AcceptSuccess,
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeEndpointResponse_endpointStatus
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeImage' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageCreated :: Core.Wait DescribeImage
newImageCreated =
  Core.Wait
    { Core.name = "ImageCreated",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "CREATED"
            Core.AcceptSuccess
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeImageVersion' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageVersionDeleted :: Core.Wait DescribeImageVersion
newImageVersionDeleted =
  Core.Wait
    { Core.name = "ImageVersionDeleted",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeImageVersionResponse_imageVersionStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
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
    { Core.name = "NotebookInstanceDeleted",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchError
            "ValidationException"
            Core.AcceptSuccess,
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeTrainingJob' every 120 seconds until a successful state is reached. An error is returned after 180 failed checks.
newTrainingJobCompletedOrStopped :: Core.Wait DescribeTrainingJob
newTrainingJobCompletedOrStopped =
  Core.Wait
    { Core.name =
        "TrainingJobCompletedOrStopped",
      Core.attempts = 180,
      Core.delay = 120,
      Core.acceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeTrainingJobResponse_trainingJobStatus
                Prelude.. Lens.to Data.toTextCI
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
    { Core.name = "NotebookInstanceInService",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "InService"
            Core.AcceptSuccess
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeNotebookInstanceResponse_notebookInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeImage' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newImageUpdated :: Core.Wait DescribeImage
newImageUpdated =
  Core.Wait
    { Core.name = "ImageUpdated",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "CREATED"
            Core.AcceptSuccess
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
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
    { Core.name = "ImageDeleted",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( describeImageResponse_imageStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
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
    { Core.name = "ImageVersionCreated",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "CREATED"
            Core.AcceptSuccess
            ( describeImageVersionResponse_imageVersionStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeImageVersionResponse_imageVersionStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
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
    { Core.name =
        "TransformJobCompletedOrStopped",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeTransformJobResponse_transformJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.SageMaker.DescribeProcessingJob' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newProcessingJobCompletedOrStopped :: Core.Wait DescribeProcessingJob
newProcessingJobCompletedOrStopped =
  Core.Wait
    { Core.name =
        "ProcessingJobCompletedOrStopped",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptSuccess
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeProcessingJobResponse_processingJobStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationException"
            Core.AcceptFailure
        ]
    }
