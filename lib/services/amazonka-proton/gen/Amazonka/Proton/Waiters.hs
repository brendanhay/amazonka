{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Proton.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.GetEnvironment
import Amazonka.Proton.GetEnvironmentTemplateVersion
import Amazonka.Proton.GetService
import Amazonka.Proton.GetServiceInstance
import Amazonka.Proton.GetServiceTemplateVersion
import Amazonka.Proton.Lens
import Amazonka.Proton.Types

-- | Polls 'Amazonka.Proton.GetServiceTemplateVersion' every 2 seconds until a successful state is reached. An error is returned after 150 failed checks.
newServiceTemplateVersionRegistered :: Core.Wait GetServiceTemplateVersion
newServiceTemplateVersionRegistered =
  Core.Wait
    { Core._waitName =
        "ServiceTemplateVersionRegistered",
      Core._waitAttempts = 150,
      Core._waitDelay = 2,
      Core._waitAcceptors =
        [ Core.matchAll
            "DRAFT"
            Core.AcceptSuccess
            ( getServiceTemplateVersionResponse_serviceTemplateVersion
                Prelude.. serviceTemplateVersion_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "PUBLISHED"
            Core.AcceptSuccess
            ( getServiceTemplateVersionResponse_serviceTemplateVersion
                Prelude.. serviceTemplateVersion_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "REGISTRATION_FAILED"
            Core.AcceptFailure
            ( getServiceTemplateVersionResponse_serviceTemplateVersion
                Prelude.. serviceTemplateVersion_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetEnvironment' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newEnvironmentDeployed :: Core.Wait GetEnvironment
newEnvironmentDeployed =
  Core.Wait
    { Core._waitName = "EnvironmentDeployed",
      Core._waitAttempts = 999,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "SUCCEEDED"
            Core.AcceptSuccess
            ( getEnvironmentResponse_environment
                Prelude.. environment_deploymentStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getEnvironmentResponse_environment
                Prelude.. environment_deploymentStatus
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetEnvironmentTemplateVersion' every 2 seconds until a successful state is reached. An error is returned after 150 failed checks.
newEnvironmentTemplateVersionRegistered :: Core.Wait GetEnvironmentTemplateVersion
newEnvironmentTemplateVersionRegistered =
  Core.Wait
    { Core._waitName =
        "EnvironmentTemplateVersionRegistered",
      Core._waitAttempts = 150,
      Core._waitDelay = 2,
      Core._waitAcceptors =
        [ Core.matchAll
            "DRAFT"
            Core.AcceptSuccess
            ( getEnvironmentTemplateVersionResponse_environmentTemplateVersion
                Prelude.. environmentTemplateVersion_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "PUBLISHED"
            Core.AcceptSuccess
            ( getEnvironmentTemplateVersionResponse_environmentTemplateVersion
                Prelude.. environmentTemplateVersion_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "REGISTRATION_FAILED"
            Core.AcceptFailure
            ( getEnvironmentTemplateVersionResponse_environmentTemplateVersion
                Prelude.. environmentTemplateVersion_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceCreated :: Core.Wait GetService
newServiceCreated =
  Core.Wait
    { Core._waitName = "ServiceCreated",
      Core._waitAttempts = 999,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED_CLEANUP_COMPLETE"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED_CLEANUP_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceDeleted :: Core.Wait GetService
newServiceDeleted =
  Core.Wait
    { Core._waitName = "ServiceDeleted",
      Core._waitAttempts = 999,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceUpdated :: Core.Wait GetService
newServiceUpdated =
  Core.Wait
    { Core._waitName = "ServiceUpdated",
      Core._waitAttempts = 999,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED_CLEANUP_COMPLETE"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED_CLEANUP_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "UPDATE_COMPLETE_CLEANUP_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetServiceInstance' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceInstanceDeployed :: Core.Wait GetServiceInstance
newServiceInstanceDeployed =
  Core.Wait
    { Core._waitName =
        "ServiceInstanceDeployed",
      Core._waitAttempts = 999,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "SUCCEEDED"
            Core.AcceptSuccess
            ( getServiceInstanceResponse_serviceInstance
                Prelude.. serviceInstance_deploymentStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getServiceInstanceResponse_serviceInstance
                Prelude.. serviceInstance_deploymentStatus
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 10 seconds until a successful state is reached. An error is returned after 360 failed checks.
newServicePipelineDeployed :: Core.Wait GetService
newServicePipelineDeployed =
  Core.Wait
    { Core._waitName =
        "ServicePipelineDeployed",
      Core._waitAttempts = 360,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "SUCCEEDED"
            Core.AcceptSuccess
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_pipeline
                Prelude.. Lens._Just
                Prelude.. servicePipeline_deploymentStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_pipeline
                Prelude.. Lens._Just
                Prelude.. servicePipeline_deploymentStatus
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
