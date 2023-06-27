{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.DomainSettingsForUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DomainSettingsForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ExecutionRoleIdentityConfig
import Amazonka.SageMaker.Types.RStudioServerProDomainSettingsForUpdate

-- | A collection of @Domain@ configuration settings to update.
--
-- /See:/ 'newDomainSettingsForUpdate' smart constructor.
data DomainSettingsForUpdate = DomainSettingsForUpdate'
  { -- | The configuration for attaching a SageMaker user profile name to the
    -- execution role as a
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html sts:SourceIdentity key>.
    -- This configuration can only be modified if there are no apps in the
    -- @InService@ or @Pending@ state.
    executionRoleIdentityConfig :: Prelude.Maybe ExecutionRoleIdentityConfig,
    -- | A collection of @RStudioServerPro@ Domain-level app settings to update.
    -- A single @RStudioServerPro@ application is created for a domain.
    rStudioServerProDomainSettingsForUpdate :: Prelude.Maybe RStudioServerProDomainSettingsForUpdate,
    -- | The security groups for the Amazon Virtual Private Cloud that the
    -- @Domain@ uses for communication between Domain-level apps and user apps.
    securityGroupIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainSettingsForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionRoleIdentityConfig', 'domainSettingsForUpdate_executionRoleIdentityConfig' - The configuration for attaching a SageMaker user profile name to the
-- execution role as a
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html sts:SourceIdentity key>.
-- This configuration can only be modified if there are no apps in the
-- @InService@ or @Pending@ state.
--
-- 'rStudioServerProDomainSettingsForUpdate', 'domainSettingsForUpdate_rStudioServerProDomainSettingsForUpdate' - A collection of @RStudioServerPro@ Domain-level app settings to update.
-- A single @RStudioServerPro@ application is created for a domain.
--
-- 'securityGroupIds', 'domainSettingsForUpdate_securityGroupIds' - The security groups for the Amazon Virtual Private Cloud that the
-- @Domain@ uses for communication between Domain-level apps and user apps.
newDomainSettingsForUpdate ::
  DomainSettingsForUpdate
newDomainSettingsForUpdate =
  DomainSettingsForUpdate'
    { executionRoleIdentityConfig =
        Prelude.Nothing,
      rStudioServerProDomainSettingsForUpdate =
        Prelude.Nothing,
      securityGroupIds = Prelude.Nothing
    }

-- | The configuration for attaching a SageMaker user profile name to the
-- execution role as a
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html sts:SourceIdentity key>.
-- This configuration can only be modified if there are no apps in the
-- @InService@ or @Pending@ state.
domainSettingsForUpdate_executionRoleIdentityConfig :: Lens.Lens' DomainSettingsForUpdate (Prelude.Maybe ExecutionRoleIdentityConfig)
domainSettingsForUpdate_executionRoleIdentityConfig = Lens.lens (\DomainSettingsForUpdate' {executionRoleIdentityConfig} -> executionRoleIdentityConfig) (\s@DomainSettingsForUpdate' {} a -> s {executionRoleIdentityConfig = a} :: DomainSettingsForUpdate)

-- | A collection of @RStudioServerPro@ Domain-level app settings to update.
-- A single @RStudioServerPro@ application is created for a domain.
domainSettingsForUpdate_rStudioServerProDomainSettingsForUpdate :: Lens.Lens' DomainSettingsForUpdate (Prelude.Maybe RStudioServerProDomainSettingsForUpdate)
domainSettingsForUpdate_rStudioServerProDomainSettingsForUpdate = Lens.lens (\DomainSettingsForUpdate' {rStudioServerProDomainSettingsForUpdate} -> rStudioServerProDomainSettingsForUpdate) (\s@DomainSettingsForUpdate' {} a -> s {rStudioServerProDomainSettingsForUpdate = a} :: DomainSettingsForUpdate)

-- | The security groups for the Amazon Virtual Private Cloud that the
-- @Domain@ uses for communication between Domain-level apps and user apps.
domainSettingsForUpdate_securityGroupIds :: Lens.Lens' DomainSettingsForUpdate (Prelude.Maybe [Prelude.Text])
domainSettingsForUpdate_securityGroupIds = Lens.lens (\DomainSettingsForUpdate' {securityGroupIds} -> securityGroupIds) (\s@DomainSettingsForUpdate' {} a -> s {securityGroupIds = a} :: DomainSettingsForUpdate) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable DomainSettingsForUpdate where
  hashWithSalt _salt DomainSettingsForUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` executionRoleIdentityConfig
      `Prelude.hashWithSalt` rStudioServerProDomainSettingsForUpdate
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData DomainSettingsForUpdate where
  rnf DomainSettingsForUpdate' {..} =
    Prelude.rnf executionRoleIdentityConfig
      `Prelude.seq` Prelude.rnf rStudioServerProDomainSettingsForUpdate
      `Prelude.seq` Prelude.rnf securityGroupIds

instance Data.ToJSON DomainSettingsForUpdate where
  toJSON DomainSettingsForUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExecutionRoleIdentityConfig" Data..=)
              Prelude.<$> executionRoleIdentityConfig,
            ("RStudioServerProDomainSettingsForUpdate" Data..=)
              Prelude.<$> rStudioServerProDomainSettingsForUpdate,
            ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds
          ]
      )
