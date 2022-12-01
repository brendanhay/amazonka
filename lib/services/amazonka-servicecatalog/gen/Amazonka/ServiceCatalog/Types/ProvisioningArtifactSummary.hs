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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a provisioning artifact (also known as a
-- version) for a product.
--
-- /See:/ 'newProvisioningArtifactSummary' smart constructor.
data ProvisioningArtifactSummary = ProvisioningArtifactSummary'
  { -- | The name of the provisioning artifact.
    name :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the provisioning artifact.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the provisioning artifact.
    description :: Prelude.Maybe Prelude.Text,
    -- | The metadata for the provisioning artifact. This is used with Amazon Web
    -- Services Marketplace products.
    provisioningArtifactMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'name', 'provisioningArtifactSummary_name' - The name of the provisioning artifact.
--
-- 'createdTime', 'provisioningArtifactSummary_createdTime' - The UTC time stamp of the creation time.
--
-- 'id', 'provisioningArtifactSummary_id' - The identifier of the provisioning artifact.
--
-- 'description', 'provisioningArtifactSummary_description' - The description of the provisioning artifact.
--
-- 'provisioningArtifactMetadata', 'provisioningArtifactSummary_provisioningArtifactMetadata' - The metadata for the provisioning artifact. This is used with Amazon Web
-- Services Marketplace products.
newProvisioningArtifactSummary ::
  ProvisioningArtifactSummary
newProvisioningArtifactSummary =
  ProvisioningArtifactSummary'
    { name =
        Prelude.Nothing,
      createdTime = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      provisioningArtifactMetadata = Prelude.Nothing
    }

-- | The name of the provisioning artifact.
provisioningArtifactSummary_name :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.Text)
provisioningArtifactSummary_name = Lens.lens (\ProvisioningArtifactSummary' {name} -> name) (\s@ProvisioningArtifactSummary' {} a -> s {name = a} :: ProvisioningArtifactSummary)

-- | The UTC time stamp of the creation time.
provisioningArtifactSummary_createdTime :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.UTCTime)
provisioningArtifactSummary_createdTime = Lens.lens (\ProvisioningArtifactSummary' {createdTime} -> createdTime) (\s@ProvisioningArtifactSummary' {} a -> s {createdTime = a} :: ProvisioningArtifactSummary) Prelude.. Lens.mapping Core._Time

-- | The identifier of the provisioning artifact.
provisioningArtifactSummary_id :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.Text)
provisioningArtifactSummary_id = Lens.lens (\ProvisioningArtifactSummary' {id} -> id) (\s@ProvisioningArtifactSummary' {} a -> s {id = a} :: ProvisioningArtifactSummary)

-- | The description of the provisioning artifact.
provisioningArtifactSummary_description :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe Prelude.Text)
provisioningArtifactSummary_description = Lens.lens (\ProvisioningArtifactSummary' {description} -> description) (\s@ProvisioningArtifactSummary' {} a -> s {description = a} :: ProvisioningArtifactSummary)

-- | The metadata for the provisioning artifact. This is used with Amazon Web
-- Services Marketplace products.
provisioningArtifactSummary_provisioningArtifactMetadata :: Lens.Lens' ProvisioningArtifactSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
provisioningArtifactSummary_provisioningArtifactMetadata = Lens.lens (\ProvisioningArtifactSummary' {provisioningArtifactMetadata} -> provisioningArtifactMetadata) (\s@ProvisioningArtifactSummary' {} a -> s {provisioningArtifactMetadata = a} :: ProvisioningArtifactSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ProvisioningArtifactSummary where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactSummary"
      ( \x ->
          ProvisioningArtifactSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> ( x Core..:? "ProvisioningArtifactMetadata"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ProvisioningArtifactSummary where
  hashWithSalt _salt ProvisioningArtifactSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` provisioningArtifactMetadata

instance Prelude.NFData ProvisioningArtifactSummary where
  rnf ProvisioningArtifactSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf provisioningArtifactMetadata
