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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactGuidance
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactType

-- | Information about a provisioning artifact (also known as a version) for
-- a product.
--
-- /See:/ 'newProvisioningArtifactDetail' smart constructor.
data ProvisioningArtifactDetail = ProvisioningArtifactDetail'
  { -- | The name of the provisioning artifact.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of provisioning artifact.
    --
    -- -   @CLOUD_FORMATION_TEMPLATE@ - CloudFormation template
    --
    -- -   @MARKETPLACE_AMI@ - Amazon Web Services Marketplace AMI
    --
    -- -   @MARKETPLACE_CAR@ - Amazon Web Services Marketplace Clusters and
    --     Amazon Web Services Resources
    type' :: Prelude.Maybe ProvisioningArtifactType,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether the product version is active.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the provisioning artifact.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the provisioning artifact.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information set by the administrator to provide guidance to end users
    -- about which provisioning artifacts to use.
    guidance :: Prelude.Maybe ProvisioningArtifactGuidance,
    -- | Specifies the revision of the external artifact that was used to
    -- automatically sync the Service Catalog product and create the
    -- provisioning artifact. Service Catalog includes this response parameter
    -- as a high level field to the existing @ProvisioningArtifactDetail@ type,
    -- which is returned as part of the response for @CreateProduct@,
    -- @UpdateProduct@, @DescribeProductAsAdmin@,
    -- @DescribeProvisioningArtifact@, @ListProvisioningArtifact@, and
    -- @UpdateProvisioningArticat@ APIs.
    --
    -- This field only exists for Repo-Synced products.
    sourceRevision :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'provisioningArtifactDetail_name' - The name of the provisioning artifact.
--
-- 'type'', 'provisioningArtifactDetail_type' - The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - Amazon Web Services Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - Amazon Web Services Marketplace Clusters and
--     Amazon Web Services Resources
--
-- 'createdTime', 'provisioningArtifactDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'active', 'provisioningArtifactDetail_active' - Indicates whether the product version is active.
--
-- 'id', 'provisioningArtifactDetail_id' - The identifier of the provisioning artifact.
--
-- 'description', 'provisioningArtifactDetail_description' - The description of the provisioning artifact.
--
-- 'guidance', 'provisioningArtifactDetail_guidance' - Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
--
-- 'sourceRevision', 'provisioningArtifactDetail_sourceRevision' - Specifies the revision of the external artifact that was used to
-- automatically sync the Service Catalog product and create the
-- provisioning artifact. Service Catalog includes this response parameter
-- as a high level field to the existing @ProvisioningArtifactDetail@ type,
-- which is returned as part of the response for @CreateProduct@,
-- @UpdateProduct@, @DescribeProductAsAdmin@,
-- @DescribeProvisioningArtifact@, @ListProvisioningArtifact@, and
-- @UpdateProvisioningArticat@ APIs.
--
-- This field only exists for Repo-Synced products.
newProvisioningArtifactDetail ::
  ProvisioningArtifactDetail
newProvisioningArtifactDetail =
  ProvisioningArtifactDetail'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      active = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      guidance = Prelude.Nothing,
      sourceRevision = Prelude.Nothing
    }

-- | The name of the provisioning artifact.
provisioningArtifactDetail_name :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Text)
provisioningArtifactDetail_name = Lens.lens (\ProvisioningArtifactDetail' {name} -> name) (\s@ProvisioningArtifactDetail' {} a -> s {name = a} :: ProvisioningArtifactDetail)

-- | The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - Amazon Web Services Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - Amazon Web Services Marketplace Clusters and
--     Amazon Web Services Resources
provisioningArtifactDetail_type :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe ProvisioningArtifactType)
provisioningArtifactDetail_type = Lens.lens (\ProvisioningArtifactDetail' {type'} -> type') (\s@ProvisioningArtifactDetail' {} a -> s {type' = a} :: ProvisioningArtifactDetail)

-- | The UTC time stamp of the creation time.
provisioningArtifactDetail_createdTime :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.UTCTime)
provisioningArtifactDetail_createdTime = Lens.lens (\ProvisioningArtifactDetail' {createdTime} -> createdTime) (\s@ProvisioningArtifactDetail' {} a -> s {createdTime = a} :: ProvisioningArtifactDetail) Prelude.. Lens.mapping Data._Time

-- | Indicates whether the product version is active.
provisioningArtifactDetail_active :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Bool)
provisioningArtifactDetail_active = Lens.lens (\ProvisioningArtifactDetail' {active} -> active) (\s@ProvisioningArtifactDetail' {} a -> s {active = a} :: ProvisioningArtifactDetail)

-- | The identifier of the provisioning artifact.
provisioningArtifactDetail_id :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Text)
provisioningArtifactDetail_id = Lens.lens (\ProvisioningArtifactDetail' {id} -> id) (\s@ProvisioningArtifactDetail' {} a -> s {id = a} :: ProvisioningArtifactDetail)

-- | The description of the provisioning artifact.
provisioningArtifactDetail_description :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Text)
provisioningArtifactDetail_description = Lens.lens (\ProvisioningArtifactDetail' {description} -> description) (\s@ProvisioningArtifactDetail' {} a -> s {description = a} :: ProvisioningArtifactDetail)

-- | Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
provisioningArtifactDetail_guidance :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe ProvisioningArtifactGuidance)
provisioningArtifactDetail_guidance = Lens.lens (\ProvisioningArtifactDetail' {guidance} -> guidance) (\s@ProvisioningArtifactDetail' {} a -> s {guidance = a} :: ProvisioningArtifactDetail)

-- | Specifies the revision of the external artifact that was used to
-- automatically sync the Service Catalog product and create the
-- provisioning artifact. Service Catalog includes this response parameter
-- as a high level field to the existing @ProvisioningArtifactDetail@ type,
-- which is returned as part of the response for @CreateProduct@,
-- @UpdateProduct@, @DescribeProductAsAdmin@,
-- @DescribeProvisioningArtifact@, @ListProvisioningArtifact@, and
-- @UpdateProvisioningArticat@ APIs.
--
-- This field only exists for Repo-Synced products.
provisioningArtifactDetail_sourceRevision :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Text)
provisioningArtifactDetail_sourceRevision = Lens.lens (\ProvisioningArtifactDetail' {sourceRevision} -> sourceRevision) (\s@ProvisioningArtifactDetail' {} a -> s {sourceRevision = a} :: ProvisioningArtifactDetail)

instance Data.FromJSON ProvisioningArtifactDetail where
  parseJSON =
    Data.withObject
      "ProvisioningArtifactDetail"
      ( \x ->
          ProvisioningArtifactDetail'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Active")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Guidance")
            Prelude.<*> (x Data..:? "SourceRevision")
      )

instance Prelude.Hashable ProvisioningArtifactDetail where
  hashWithSalt _salt ProvisioningArtifactDetail' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` guidance
      `Prelude.hashWithSalt` sourceRevision

instance Prelude.NFData ProvisioningArtifactDetail where
  rnf ProvisioningArtifactDetail' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf active
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf guidance
      `Prelude.seq` Prelude.rnf sourceRevision
