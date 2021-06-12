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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provisioning artifact output.
--
-- /See:/ 'newProvisioningArtifactOutput' smart constructor.
data ProvisioningArtifactOutput = ProvisioningArtifactOutput'
  { -- | The provisioning artifact output key.
    key :: Core.Maybe Core.Text,
    -- | Description of the provisioning artifact output key.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningArtifactOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'provisioningArtifactOutput_key' - The provisioning artifact output key.
--
-- 'description', 'provisioningArtifactOutput_description' - Description of the provisioning artifact output key.
newProvisioningArtifactOutput ::
  ProvisioningArtifactOutput
newProvisioningArtifactOutput =
  ProvisioningArtifactOutput'
    { key = Core.Nothing,
      description = Core.Nothing
    }

-- | The provisioning artifact output key.
provisioningArtifactOutput_key :: Lens.Lens' ProvisioningArtifactOutput (Core.Maybe Core.Text)
provisioningArtifactOutput_key = Lens.lens (\ProvisioningArtifactOutput' {key} -> key) (\s@ProvisioningArtifactOutput' {} a -> s {key = a} :: ProvisioningArtifactOutput)

-- | Description of the provisioning artifact output key.
provisioningArtifactOutput_description :: Lens.Lens' ProvisioningArtifactOutput (Core.Maybe Core.Text)
provisioningArtifactOutput_description = Lens.lens (\ProvisioningArtifactOutput' {description} -> description) (\s@ProvisioningArtifactOutput' {} a -> s {description = a} :: ProvisioningArtifactOutput)

instance Core.FromJSON ProvisioningArtifactOutput where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactOutput"
      ( \x ->
          ProvisioningArtifactOutput'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable ProvisioningArtifactOutput

instance Core.NFData ProvisioningArtifactOutput
