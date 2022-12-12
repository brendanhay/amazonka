{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Omics.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.GetAnnotationImportJob
import Amazonka.Omics.GetAnnotationStore
import Amazonka.Omics.GetReadSetActivationJob
import Amazonka.Omics.GetReadSetExportJob
import Amazonka.Omics.GetReadSetImportJob
import Amazonka.Omics.GetReferenceImportJob
import Amazonka.Omics.GetRun
import Amazonka.Omics.GetRunTask
import Amazonka.Omics.GetVariantImportJob
import Amazonka.Omics.GetVariantStore
import Amazonka.Omics.GetWorkflow
import Amazonka.Omics.Lens
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.Omics.GetAnnotationImportJob' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAnnotationImportJobCreated :: Core.Wait GetAnnotationImportJob
newAnnotationImportJobCreated =
  Core.Wait
    { Core.name = "AnnotationImportJobCreated",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "SUBMITTED"
            Core.AcceptRetry
            ( getAnnotationImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "IN_PROGRESS"
            Core.AcceptRetry
            ( getAnnotationImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getAnnotationImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptSuccess
            ( getAnnotationImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getAnnotationImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetAnnotationStore' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAnnotationStoreCreated :: Core.Wait GetAnnotationStore
newAnnotationStoreCreated =
  Core.Wait
    { Core.name = "AnnotationStoreCreated",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getAnnotationStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( getAnnotationStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATING"
            Core.AcceptRetry
            ( getAnnotationStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getAnnotationStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetAnnotationStore' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAnnotationStoreDeleted :: Core.Wait GetAnnotationStore
newAnnotationStoreDeleted =
  Core.Wait
    { Core.name = "AnnotationStoreDeleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( getAnnotationStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( getAnnotationStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetReadSetActivationJob' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newReadSetActivationJobCompleted :: Core.Wait GetReadSetActivationJob
newReadSetActivationJobCompleted =
  Core.Wait
    { Core.name =
        "ReadSetActivationJobCompleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getReadSetActivationJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "SUBMITTED"
            Core.AcceptRetry
            ( getReadSetActivationJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "IN_PROGRESS"
            Core.AcceptRetry
            ( getReadSetActivationJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLING"
            Core.AcceptRetry
            ( getReadSetActivationJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( getReadSetActivationJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReadSetActivationJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "COMPLETED_WITH_FAILURES"
            Core.AcceptFailure
            ( getReadSetActivationJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetReadSetExportJob' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newReadSetExportJobCompleted :: Core.Wait GetReadSetExportJob
newReadSetExportJobCompleted =
  Core.Wait
    { Core.name = "ReadSetExportJobCompleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getReadSetExportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "SUBMITTED"
            Core.AcceptRetry
            ( getReadSetExportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "IN_PROGRESS"
            Core.AcceptRetry
            ( getReadSetExportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLING"
            Core.AcceptRetry
            ( getReadSetExportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( getReadSetExportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReadSetExportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "COMPLETED_WITH_FAILURES"
            Core.AcceptFailure
            ( getReadSetExportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetReadSetImportJob' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newReadSetImportJobCompleted :: Core.Wait GetReadSetImportJob
newReadSetImportJobCompleted =
  Core.Wait
    { Core.name = "ReadSetImportJobCompleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getReadSetImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "SUBMITTED"
            Core.AcceptRetry
            ( getReadSetImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "IN_PROGRESS"
            Core.AcceptRetry
            ( getReadSetImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLING"
            Core.AcceptRetry
            ( getReadSetImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( getReadSetImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReadSetImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "COMPLETED_WITH_FAILURES"
            Core.AcceptFailure
            ( getReadSetImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetReferenceImportJob' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newReferenceImportJobCompleted :: Core.Wait GetReferenceImportJob
newReferenceImportJobCompleted =
  Core.Wait
    { Core.name =
        "ReferenceImportJobCompleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getReferenceImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "SUBMITTED"
            Core.AcceptRetry
            ( getReferenceImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "IN_PROGRESS"
            Core.AcceptRetry
            ( getReferenceImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLING"
            Core.AcceptRetry
            ( getReferenceImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( getReferenceImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReferenceImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "COMPLETED_WITH_FAILURES"
            Core.AcceptFailure
            ( getReferenceImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetRun' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newRunCompleted :: Core.Wait GetRun
newRunCompleted =
  Core.Wait
    { Core.name = "RunCompleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "PENDING"
            Core.AcceptRetry
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "RUNNING"
            Core.AcceptRetry
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STOPPING"
            Core.AcceptRetry
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetRun' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newRunRunning :: Core.Wait GetRun
newRunRunning =
  Core.Wait
    { Core.name = "RunRunning",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "PENDING"
            Core.AcceptRetry
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( getRunResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetRunTask' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newTaskCompleted :: Core.Wait GetRunTask
newTaskCompleted =
  Core.Wait
    { Core.name = "TaskCompleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "PENDING"
            Core.AcceptRetry
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "RUNNING"
            Core.AcceptRetry
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STOPPING"
            Core.AcceptRetry
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetRunTask' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newTaskRunning :: Core.Wait GetRunTask
newTaskRunning =
  Core.Wait
    { Core.name = "TaskRunning",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "PENDING"
            Core.AcceptRetry
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( getRunTaskResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetVariantImportJob' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newVariantImportJobCreated :: Core.Wait GetVariantImportJob
newVariantImportJobCreated =
  Core.Wait
    { Core.name = "VariantImportJobCreated",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "SUBMITTED"
            Core.AcceptRetry
            ( getVariantImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "IN_PROGRESS"
            Core.AcceptRetry
            ( getVariantImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getVariantImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptSuccess
            ( getVariantImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( getVariantImportJobResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetVariantStore' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newVariantStoreCreated :: Core.Wait GetVariantStore
newVariantStoreCreated =
  Core.Wait
    { Core.name = "VariantStoreCreated",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getVariantStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( getVariantStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATING"
            Core.AcceptRetry
            ( getVariantStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getVariantStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetVariantStore' every 30 seconds until a successful state is reached. An error is returned after 20 failed checks.
newVariantStoreDeleted :: Core.Wait GetVariantStore
newVariantStoreDeleted =
  Core.Wait
    { Core.name = "VariantStoreDeleted",
      Core.attempts = 20,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( getVariantStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( getVariantStoreResponse_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Omics.GetWorkflow' every 3 seconds until a successful state is reached. An error is returned after 10 failed checks.
newWorkflowActive :: Core.Wait GetWorkflow
newWorkflowActive =
  Core.Wait
    { Core.name = "WorkflowActive",
      Core.attempts = 10,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getWorkflowResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( getWorkflowResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATING"
            Core.AcceptRetry
            ( getWorkflowResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getWorkflowResponse_status Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
