{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Proton.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.GetComponent
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
    { Core.name =
        "ServiceTemplateVersionRegistered",
      Core.attempts = 150,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "DRAFT"
            Core.AcceptSuccess
            ( getServiceTemplateVersionResponse_serviceTemplateVersion
                Prelude.. serviceTemplateVersion_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "PUBLISHED"
            Core.AcceptSuccess
            ( getServiceTemplateVersionResponse_serviceTemplateVersion
                Prelude.. serviceTemplateVersion_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "REGISTRATION_FAILED"
            Core.AcceptFailure
            ( getServiceTemplateVersionResponse_serviceTemplateVersion
                Prelude.. serviceTemplateVersion_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetEnvironment' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newEnvironmentDeployed :: Core.Wait GetEnvironment
newEnvironmentDeployed =
  Core.Wait
    { Core.name = "EnvironmentDeployed",
      Core.attempts = 999,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "SUCCEEDED"
            Core.AcceptSuccess
            ( getEnvironmentResponse_environment
                Prelude.. environment_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getEnvironmentResponse_environment
                Prelude.. environment_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetEnvironmentTemplateVersion' every 2 seconds until a successful state is reached. An error is returned after 150 failed checks.
newEnvironmentTemplateVersionRegistered :: Core.Wait GetEnvironmentTemplateVersion
newEnvironmentTemplateVersionRegistered =
  Core.Wait
    { Core.name =
        "EnvironmentTemplateVersionRegistered",
      Core.attempts = 150,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "DRAFT"
            Core.AcceptSuccess
            ( getEnvironmentTemplateVersionResponse_environmentTemplateVersion
                Prelude.. environmentTemplateVersion_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "PUBLISHED"
            Core.AcceptSuccess
            ( getEnvironmentTemplateVersionResponse_environmentTemplateVersion
                Prelude.. environmentTemplateVersion_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "REGISTRATION_FAILED"
            Core.AcceptFailure
            ( getEnvironmentTemplateVersionResponse_environmentTemplateVersion
                Prelude.. environmentTemplateVersion_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceCreated :: Core.Wait GetService
newServiceCreated =
  Core.Wait
    { Core.name = "ServiceCreated",
      Core.attempts = 999,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED_CLEANUP_COMPLETE"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED_CLEANUP_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceDeleted :: Core.Wait GetService
newServiceDeleted =
  Core.Wait
    { Core.name = "ServiceDeleted",
      Core.attempts = 999,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceUpdated :: Core.Wait GetService
newServiceUpdated =
  Core.Wait
    { Core.name = "ServiceUpdated",
      Core.attempts = 999,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED_CLEANUP_COMPLETE"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED_CLEANUP_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_COMPLETE_CLEANUP_FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetServiceInstance' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newServiceInstanceDeployed :: Core.Wait GetServiceInstance
newServiceInstanceDeployed =
  Core.Wait
    { Core.name = "ServiceInstanceDeployed",
      Core.attempts = 999,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "SUCCEEDED"
            Core.AcceptSuccess
            ( getServiceInstanceResponse_serviceInstance
                Prelude.. serviceInstance_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getServiceInstanceResponse_serviceInstance
                Prelude.. serviceInstance_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetComponent' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newComponentDeployed :: Core.Wait GetComponent
newComponentDeployed =
  Core.Wait
    { Core.name = "ComponentDeployed",
      Core.attempts = 999,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "SUCCEEDED"
            Core.AcceptSuccess
            ( getComponentResponse_component Prelude.. Lens._Just
                Prelude.. component_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getComponentResponse_component Prelude.. Lens._Just
                Prelude.. component_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetService' every 10 seconds until a successful state is reached. An error is returned after 360 failed checks.
newServicePipelineDeployed :: Core.Wait GetService
newServicePipelineDeployed =
  Core.Wait
    { Core.name = "ServicePipelineDeployed",
      Core.attempts = 360,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "SUCCEEDED"
            Core.AcceptSuccess
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_pipeline
                Prelude.. Lens._Just
                Prelude.. servicePipeline_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getServiceResponse_service Prelude.. Lens._Just
                Prelude.. service_pipeline
                Prelude.. Lens._Just
                Prelude.. servicePipeline_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Proton.GetComponent' every 5 seconds until a successful state is reached. An error is returned after 999 failed checks.
newComponentDeleted :: Core.Wait GetComponent
newComponentDeleted =
  Core.Wait
    { Core.name = "ComponentDeleted",
      Core.attempts = 999,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getComponentResponse_component Prelude.. Lens._Just
                Prelude.. component_deploymentStatus
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
