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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary information about a provisioning artifact (also known as a
-- version) for a product.
--
-- /See:/ 'newProvisioningArtifactSummary' smart constructor.
data ProvisioningArtifactSummary = ProvisioningArtifactSummary'
  { -- | The identifier of the provisioning artifact.
    id :: Core.Maybe Core.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The metadata for the provisioning artifact. This is used with AWS
    -- Marketplace products.
    provisioningArtifactMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the provisioning artifact.
    name :: Core.Maybe Core.Text,
    -- | The description of the provisioning artifact.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningArtifactSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'provisioningArtifactSummary_id' - The identifier of the provisioning artifact.
--
-- 'createdTime', 'provisioningArtifactSummary_createdTime' - The UTC time stamp of the creation time.
--
-- 'provisioningArtifactMetadata', 'provisioningArtifactSummary_provisioningArtifactMetadata' - The metadata for the provisioning artifact. This is used with AWS
-- Marketplace products.
--
-- 'name', 'provisioningArtifactSummary_name' - The name of the provisioning artifact.
--
-- 'description', 'provisioningArtifactSummary_description' - The description of the provisioning artifact.
newProvisioningArtifactSummary ::
  ProvisioningArtifactSummary
newProvisioningArtifactSummary =
  ProvisioningArtifactSummary'
    { id = Core.Nothing,
      createdTime = Core.Nothing,
      provisioningArtifactMetadata = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing
    }

-- | The identifier of the provisioning artifact.
provisioningArtifactSummary_id :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Core.Text)
provisioningArtifactSummary_id = Lens.lens (\ProvisioningArtifactSummary' {id} -> id) (\s@ProvisioningArtifactSummary' {} a -> s {id = a} :: ProvisioningArtifactSummary)

-- | The UTC time stamp of the creation time.
provisioningArtifactSummary_createdTime :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Core.UTCTime)
provisioningArtifactSummary_createdTime = Lens.lens (\ProvisioningArtifactSummary' {createdTime} -> createdTime) (\s@ProvisioningArtifactSummary' {} a -> s {createdTime = a} :: ProvisioningArtifactSummary) Core.. Lens.mapping Core._Time

-- | The metadata for the provisioning artifact. This is used with AWS
-- Marketplace products.
provisioningArtifactSummary_provisioningArtifactMetadata :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe (Core.HashMap Core.Text Core.Text))
provisioningArtifactSummary_provisioningArtifactMetadata = Lens.lens (\ProvisioningArtifactSummary' {provisioningArtifactMetadata} -> provisioningArtifactMetadata) (\s@ProvisioningArtifactSummary' {} a -> s {provisioningArtifactMetadata = a} :: ProvisioningArtifactSummary) Core.. Lens.mapping Lens._Coerce

-- | The name of the provisioning artifact.
provisioningArtifactSummary_name :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Core.Text)
provisioningArtifactSummary_name = Lens.lens (\ProvisioningArtifactSummary' {name} -> name) (\s@ProvisioningArtifactSummary' {} a -> s {name = a} :: ProvisioningArtifactSummary)

-- | The description of the provisioning artifact.
provisioningArtifactSummary_description :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Core.Text)
provisioningArtifactSummary_description = Lens.lens (\ProvisioningArtifactSummary' {description} -> description) (\s@ProvisioningArtifactSummary' {} a -> s {description = a} :: ProvisioningArtifactSummary)

instance Core.FromJSON ProvisioningArtifactSummary where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactSummary"
      ( \x ->
          ProvisioningArtifactSummary'
            Core.<$> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> ( x Core..:? "ProvisioningArtifactMetadata"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable ProvisioningArtifactSummary

instance Core.NFData ProvisioningArtifactSummary
