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
-- Module      : Amazonka.SageMaker.Types.DomainSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DomainSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ExecutionRoleIdentityConfig
import Amazonka.SageMaker.Types.RStudioServerProDomainSettings

-- | A collection of settings that apply to the @SageMaker Domain@. These
-- settings are specified through the @CreateDomain@ API call.
--
-- /See:/ 'newDomainSettings' smart constructor.
data DomainSettings = DomainSettings'
  { -- | The security groups for the Amazon Virtual Private Cloud that the
    -- @Domain@ uses for communication between Domain-level apps and user apps.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The configuration for attaching a SageMaker user profile name to the
    -- execution role as a
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html sts:SourceIdentity key>.
    executionRoleIdentityConfig :: Prelude.Maybe ExecutionRoleIdentityConfig,
    -- | A collection of settings that configure the @RStudioServerPro@
    -- Domain-level app.
    rStudioServerProDomainSettings :: Prelude.Maybe RStudioServerProDomainSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'domainSettings_securityGroupIds' - The security groups for the Amazon Virtual Private Cloud that the
-- @Domain@ uses for communication between Domain-level apps and user apps.
--
-- 'executionRoleIdentityConfig', 'domainSettings_executionRoleIdentityConfig' - The configuration for attaching a SageMaker user profile name to the
-- execution role as a
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html sts:SourceIdentity key>.
--
-- 'rStudioServerProDomainSettings', 'domainSettings_rStudioServerProDomainSettings' - A collection of settings that configure the @RStudioServerPro@
-- Domain-level app.
newDomainSettings ::
  DomainSettings
newDomainSettings =
  DomainSettings'
    { securityGroupIds = Prelude.Nothing,
      executionRoleIdentityConfig = Prelude.Nothing,
      rStudioServerProDomainSettings = Prelude.Nothing
    }

-- | The security groups for the Amazon Virtual Private Cloud that the
-- @Domain@ uses for communication between Domain-level apps and user apps.
domainSettings_securityGroupIds :: Lens.Lens' DomainSettings (Prelude.Maybe [Prelude.Text])
domainSettings_securityGroupIds = Lens.lens (\DomainSettings' {securityGroupIds} -> securityGroupIds) (\s@DomainSettings' {} a -> s {securityGroupIds = a} :: DomainSettings) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for attaching a SageMaker user profile name to the
-- execution role as a
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html sts:SourceIdentity key>.
domainSettings_executionRoleIdentityConfig :: Lens.Lens' DomainSettings (Prelude.Maybe ExecutionRoleIdentityConfig)
domainSettings_executionRoleIdentityConfig = Lens.lens (\DomainSettings' {executionRoleIdentityConfig} -> executionRoleIdentityConfig) (\s@DomainSettings' {} a -> s {executionRoleIdentityConfig = a} :: DomainSettings)

-- | A collection of settings that configure the @RStudioServerPro@
-- Domain-level app.
domainSettings_rStudioServerProDomainSettings :: Lens.Lens' DomainSettings (Prelude.Maybe RStudioServerProDomainSettings)
domainSettings_rStudioServerProDomainSettings = Lens.lens (\DomainSettings' {rStudioServerProDomainSettings} -> rStudioServerProDomainSettings) (\s@DomainSettings' {} a -> s {rStudioServerProDomainSettings = a} :: DomainSettings)

instance Core.FromJSON DomainSettings where
  parseJSON =
    Core.withObject
      "DomainSettings"
      ( \x ->
          DomainSettings'
            Prelude.<$> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ExecutionRoleIdentityConfig")
            Prelude.<*> (x Core..:? "RStudioServerProDomainSettings")
      )

instance Prelude.Hashable DomainSettings where
  hashWithSalt _salt DomainSettings' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` executionRoleIdentityConfig
      `Prelude.hashWithSalt` rStudioServerProDomainSettings

instance Prelude.NFData DomainSettings where
  rnf DomainSettings' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf executionRoleIdentityConfig
      `Prelude.seq` Prelude.rnf rStudioServerProDomainSettings

instance Core.ToJSON DomainSettings where
  toJSON DomainSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("ExecutionRoleIdentityConfig" Core..=)
              Prelude.<$> executionRoleIdentityConfig,
            ("RStudioServerProDomainSettings" Core..=)
              Prelude.<$> rStudioServerProDomainSettings
          ]
      )
