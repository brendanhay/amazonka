{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType

-- | Information about a provisioning artifact (also known as a version) for
-- a product.
--
-- /See:/ 'newProvisioningArtifactDetail' smart constructor.
data ProvisioningArtifactDetail = ProvisioningArtifactDetail'
  { -- | Information set by the administrator to provide guidance to end users
    -- about which provisioning artifacts to use.
    guidance :: Prelude.Maybe ProvisioningArtifactGuidance,
    -- | The identifier of the provisioning artifact.
    id :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the provisioning artifact.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the product version is active.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The description of the provisioning artifact.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of provisioning artifact.
    --
    -- -   @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
    --
    -- -   @MARKETPLACE_AMI@ - AWS Marketplace AMI
    --
    -- -   @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
    type' :: Prelude.Maybe ProvisioningArtifactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'guidance', 'provisioningArtifactDetail_guidance' - Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
--
-- 'id', 'provisioningArtifactDetail_id' - The identifier of the provisioning artifact.
--
-- 'createdTime', 'provisioningArtifactDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'name', 'provisioningArtifactDetail_name' - The name of the provisioning artifact.
--
-- 'active', 'provisioningArtifactDetail_active' - Indicates whether the product version is active.
--
-- 'description', 'provisioningArtifactDetail_description' - The description of the provisioning artifact.
--
-- 'type'', 'provisioningArtifactDetail_type' - The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
newProvisioningArtifactDetail ::
  ProvisioningArtifactDetail
newProvisioningArtifactDetail =
  ProvisioningArtifactDetail'
    { guidance =
        Prelude.Nothing,
      id = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      active = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
provisioningArtifactDetail_guidance :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe ProvisioningArtifactGuidance)
provisioningArtifactDetail_guidance = Lens.lens (\ProvisioningArtifactDetail' {guidance} -> guidance) (\s@ProvisioningArtifactDetail' {} a -> s {guidance = a} :: ProvisioningArtifactDetail)

-- | The identifier of the provisioning artifact.
provisioningArtifactDetail_id :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Text)
provisioningArtifactDetail_id = Lens.lens (\ProvisioningArtifactDetail' {id} -> id) (\s@ProvisioningArtifactDetail' {} a -> s {id = a} :: ProvisioningArtifactDetail)

-- | The UTC time stamp of the creation time.
provisioningArtifactDetail_createdTime :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.UTCTime)
provisioningArtifactDetail_createdTime = Lens.lens (\ProvisioningArtifactDetail' {createdTime} -> createdTime) (\s@ProvisioningArtifactDetail' {} a -> s {createdTime = a} :: ProvisioningArtifactDetail) Prelude.. Lens.mapping Prelude._Time

-- | The name of the provisioning artifact.
provisioningArtifactDetail_name :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Text)
provisioningArtifactDetail_name = Lens.lens (\ProvisioningArtifactDetail' {name} -> name) (\s@ProvisioningArtifactDetail' {} a -> s {name = a} :: ProvisioningArtifactDetail)

-- | Indicates whether the product version is active.
provisioningArtifactDetail_active :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Bool)
provisioningArtifactDetail_active = Lens.lens (\ProvisioningArtifactDetail' {active} -> active) (\s@ProvisioningArtifactDetail' {} a -> s {active = a} :: ProvisioningArtifactDetail)

-- | The description of the provisioning artifact.
provisioningArtifactDetail_description :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe Prelude.Text)
provisioningArtifactDetail_description = Lens.lens (\ProvisioningArtifactDetail' {description} -> description) (\s@ProvisioningArtifactDetail' {} a -> s {description = a} :: ProvisioningArtifactDetail)

-- | The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
provisioningArtifactDetail_type :: Lens.Lens' ProvisioningArtifactDetail (Prelude.Maybe ProvisioningArtifactType)
provisioningArtifactDetail_type = Lens.lens (\ProvisioningArtifactDetail' {type'} -> type') (\s@ProvisioningArtifactDetail' {} a -> s {type' = a} :: ProvisioningArtifactDetail)

instance Prelude.FromJSON ProvisioningArtifactDetail where
  parseJSON =
    Prelude.withObject
      "ProvisioningArtifactDetail"
      ( \x ->
          ProvisioningArtifactDetail'
            Prelude.<$> (x Prelude..:? "Guidance")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Active")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable ProvisioningArtifactDetail

instance Prelude.NFData ProvisioningArtifactDetail
