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
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about a provisioning artifact (also known as a
-- version) for a product.
--
-- /See:/ 'newProvisioningArtifactSummary' smart constructor.
data ProvisioningArtifactSummary = ProvisioningArtifactSummary'
  { -- | The metadata for the provisioning artifact. This is used with AWS
    -- Marketplace products.
    provisioningArtifactMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the provisioning artifact.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the provisioning artifact.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifactMetadata', 'provisioningArtifactSummary_provisioningArtifactMetadata' - The metadata for the provisioning artifact. This is used with AWS
-- Marketplace products.
--
-- 'createdTime', 'provisioningArtifactSummary_createdTime' - The UTC time stamp of the creation time.
--
-- 'name', 'provisioningArtifactSummary_name' - The name of the provisioning artifact.
--
-- 'id', 'provisioningArtifactSummary_id' - The identifier of the provisioning artifact.
--
-- 'description', 'provisioningArtifactSummary_description' - The description of the provisioning artifact.
newProvisioningArtifactSummary ::
  ProvisioningArtifactSummary
newProvisioningArtifactSummary =
  ProvisioningArtifactSummary'
    { provisioningArtifactMetadata =
        Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The metadata for the provisioning artifact. This is used with AWS
-- Marketplace products.
provisioningArtifactSummary_provisioningArtifactMetadata :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
provisioningArtifactSummary_provisioningArtifactMetadata = Lens.lens (\ProvisioningArtifactSummary' {provisioningArtifactMetadata} -> provisioningArtifactMetadata) (\s@ProvisioningArtifactSummary' {} a -> s {provisioningArtifactMetadata = a} :: ProvisioningArtifactSummary) Prelude.. Lens.mapping Lens.coerced

-- | The UTC time stamp of the creation time.
provisioningArtifactSummary_createdTime :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.UTCTime)
provisioningArtifactSummary_createdTime = Lens.lens (\ProvisioningArtifactSummary' {createdTime} -> createdTime) (\s@ProvisioningArtifactSummary' {} a -> s {createdTime = a} :: ProvisioningArtifactSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the provisioning artifact.
provisioningArtifactSummary_name :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.Text)
provisioningArtifactSummary_name = Lens.lens (\ProvisioningArtifactSummary' {name} -> name) (\s@ProvisioningArtifactSummary' {} a -> s {name = a} :: ProvisioningArtifactSummary)

-- | The identifier of the provisioning artifact.
provisioningArtifactSummary_id :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.Text)
provisioningArtifactSummary_id = Lens.lens (\ProvisioningArtifactSummary' {id} -> id) (\s@ProvisioningArtifactSummary' {} a -> s {id = a} :: ProvisioningArtifactSummary)

-- | The description of the provisioning artifact.
provisioningArtifactSummary_description :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.Text)
provisioningArtifactSummary_description = Lens.lens (\ProvisioningArtifactSummary' {description} -> description) (\s@ProvisioningArtifactSummary' {} a -> s {description = a} :: ProvisioningArtifactSummary)

instance Core.FromJSON ProvisioningArtifactSummary where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactSummary"
      ( \x ->
          ProvisioningArtifactSummary'
            Prelude.<$> ( x Core..:? "ProvisioningArtifactMetadata"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ProvisioningArtifactSummary

instance Prelude.NFData ProvisioningArtifactSummary
