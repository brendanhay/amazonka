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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifact where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance

-- | Information about a provisioning artifact. A provisioning artifact is
-- also known as a product version.
--
-- /See:/ 'newProvisioningArtifact' smart constructor.
data ProvisioningArtifact = ProvisioningArtifact'
  { -- | Information set by the administrator to provide guidance to end users
    -- about which provisioning artifacts to use.
    guidance :: Core.Maybe ProvisioningArtifactGuidance,
    -- | The identifier of the provisioning artifact.
    id :: Core.Maybe Core.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The name of the provisioning artifact.
    name :: Core.Maybe Core.Text,
    -- | The description of the provisioning artifact.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'guidance', 'provisioningArtifact_guidance' - Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
--
-- 'id', 'provisioningArtifact_id' - The identifier of the provisioning artifact.
--
-- 'createdTime', 'provisioningArtifact_createdTime' - The UTC time stamp of the creation time.
--
-- 'name', 'provisioningArtifact_name' - The name of the provisioning artifact.
--
-- 'description', 'provisioningArtifact_description' - The description of the provisioning artifact.
newProvisioningArtifact ::
  ProvisioningArtifact
newProvisioningArtifact =
  ProvisioningArtifact'
    { guidance = Core.Nothing,
      id = Core.Nothing,
      createdTime = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing
    }

-- | Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
provisioningArtifact_guidance :: Lens.Lens' ProvisioningArtifact (Core.Maybe ProvisioningArtifactGuidance)
provisioningArtifact_guidance = Lens.lens (\ProvisioningArtifact' {guidance} -> guidance) (\s@ProvisioningArtifact' {} a -> s {guidance = a} :: ProvisioningArtifact)

-- | The identifier of the provisioning artifact.
provisioningArtifact_id :: Lens.Lens' ProvisioningArtifact (Core.Maybe Core.Text)
provisioningArtifact_id = Lens.lens (\ProvisioningArtifact' {id} -> id) (\s@ProvisioningArtifact' {} a -> s {id = a} :: ProvisioningArtifact)

-- | The UTC time stamp of the creation time.
provisioningArtifact_createdTime :: Lens.Lens' ProvisioningArtifact (Core.Maybe Core.UTCTime)
provisioningArtifact_createdTime = Lens.lens (\ProvisioningArtifact' {createdTime} -> createdTime) (\s@ProvisioningArtifact' {} a -> s {createdTime = a} :: ProvisioningArtifact) Core.. Lens.mapping Core._Time

-- | The name of the provisioning artifact.
provisioningArtifact_name :: Lens.Lens' ProvisioningArtifact (Core.Maybe Core.Text)
provisioningArtifact_name = Lens.lens (\ProvisioningArtifact' {name} -> name) (\s@ProvisioningArtifact' {} a -> s {name = a} :: ProvisioningArtifact)

-- | The description of the provisioning artifact.
provisioningArtifact_description :: Lens.Lens' ProvisioningArtifact (Core.Maybe Core.Text)
provisioningArtifact_description = Lens.lens (\ProvisioningArtifact' {description} -> description) (\s@ProvisioningArtifact' {} a -> s {description = a} :: ProvisioningArtifact)

instance Core.FromJSON ProvisioningArtifact where
  parseJSON =
    Core.withObject
      "ProvisioningArtifact"
      ( \x ->
          ProvisioningArtifact'
            Core.<$> (x Core..:? "Guidance")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable ProvisioningArtifact

instance Core.NFData ProvisioningArtifact
