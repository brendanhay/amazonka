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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType

-- | Information about a provisioning artifact (also known as a version) for
-- a product.
--
-- /See:/ 'newProvisioningArtifactProperties' smart constructor.
data ProvisioningArtifactProperties = ProvisioningArtifactProperties'
  { -- | If set to true, AWS Service Catalog stops validating the specified
    -- provisioning artifact even if it is invalid.
    disableTemplateValidation :: Prelude.Maybe Prelude.Bool,
    -- | The name of the provisioning artifact (for example, v1 v2beta). No
    -- spaces are allowed.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the provisioning artifact, including how it differs
    -- from the previous provisioning artifact.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of provisioning artifact.
    --
    -- -   @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
    --
    -- -   @MARKETPLACE_AMI@ - AWS Marketplace AMI
    --
    -- -   @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
    type' :: Prelude.Maybe ProvisioningArtifactType,
    -- | Specify the template source with one of the following options, but not
    -- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ]
    --
    -- The URL of the CloudFormation template in Amazon S3. Specify the URL in
    -- JSON format as follows:
    --
    -- @\"LoadTemplateFromURL\": \"https:\/\/s3.amazonaws.com\/cf-templates-ozkq9d3hgiq2-us-east-1\/...\"@
    --
    -- @ImportFromPhysicalId@: The physical id of the resource that contains
    -- the template. Currently only supports CloudFormation stack arn. Specify
    -- the physical id in JSON format as follows:
    -- @ImportFromPhysicalId: “arn:aws:cloudformation:[us-east-1]:[accountId]:stack\/[StackName]\/[resourceId]@
    info :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableTemplateValidation', 'provisioningArtifactProperties_disableTemplateValidation' - If set to true, AWS Service Catalog stops validating the specified
-- provisioning artifact even if it is invalid.
--
-- 'name', 'provisioningArtifactProperties_name' - The name of the provisioning artifact (for example, v1 v2beta). No
-- spaces are allowed.
--
-- 'description', 'provisioningArtifactProperties_description' - The description of the provisioning artifact, including how it differs
-- from the previous provisioning artifact.
--
-- 'type'', 'provisioningArtifactProperties_type' - The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
-- 'info', 'provisioningArtifactProperties_info' - Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ]
--
-- The URL of the CloudFormation template in Amazon S3. Specify the URL in
-- JSON format as follows:
--
-- @\"LoadTemplateFromURL\": \"https:\/\/s3.amazonaws.com\/cf-templates-ozkq9d3hgiq2-us-east-1\/...\"@
--
-- @ImportFromPhysicalId@: The physical id of the resource that contains
-- the template. Currently only supports CloudFormation stack arn. Specify
-- the physical id in JSON format as follows:
-- @ImportFromPhysicalId: “arn:aws:cloudformation:[us-east-1]:[accountId]:stack\/[StackName]\/[resourceId]@
newProvisioningArtifactProperties ::
  ProvisioningArtifactProperties
newProvisioningArtifactProperties =
  ProvisioningArtifactProperties'
    { disableTemplateValidation =
        Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      info = Prelude.mempty
    }

-- | If set to true, AWS Service Catalog stops validating the specified
-- provisioning artifact even if it is invalid.
provisioningArtifactProperties_disableTemplateValidation :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe Prelude.Bool)
provisioningArtifactProperties_disableTemplateValidation = Lens.lens (\ProvisioningArtifactProperties' {disableTemplateValidation} -> disableTemplateValidation) (\s@ProvisioningArtifactProperties' {} a -> s {disableTemplateValidation = a} :: ProvisioningArtifactProperties)

-- | The name of the provisioning artifact (for example, v1 v2beta). No
-- spaces are allowed.
provisioningArtifactProperties_name :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe Prelude.Text)
provisioningArtifactProperties_name = Lens.lens (\ProvisioningArtifactProperties' {name} -> name) (\s@ProvisioningArtifactProperties' {} a -> s {name = a} :: ProvisioningArtifactProperties)

-- | The description of the provisioning artifact, including how it differs
-- from the previous provisioning artifact.
provisioningArtifactProperties_description :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe Prelude.Text)
provisioningArtifactProperties_description = Lens.lens (\ProvisioningArtifactProperties' {description} -> description) (\s@ProvisioningArtifactProperties' {} a -> s {description = a} :: ProvisioningArtifactProperties)

-- | The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
provisioningArtifactProperties_type :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe ProvisioningArtifactType)
provisioningArtifactProperties_type = Lens.lens (\ProvisioningArtifactProperties' {type'} -> type') (\s@ProvisioningArtifactProperties' {} a -> s {type' = a} :: ProvisioningArtifactProperties)

-- | Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ]
--
-- The URL of the CloudFormation template in Amazon S3. Specify the URL in
-- JSON format as follows:
--
-- @\"LoadTemplateFromURL\": \"https:\/\/s3.amazonaws.com\/cf-templates-ozkq9d3hgiq2-us-east-1\/...\"@
--
-- @ImportFromPhysicalId@: The physical id of the resource that contains
-- the template. Currently only supports CloudFormation stack arn. Specify
-- the physical id in JSON format as follows:
-- @ImportFromPhysicalId: “arn:aws:cloudformation:[us-east-1]:[accountId]:stack\/[StackName]\/[resourceId]@
provisioningArtifactProperties_info :: Lens.Lens' ProvisioningArtifactProperties (Prelude.HashMap Prelude.Text Prelude.Text)
provisioningArtifactProperties_info = Lens.lens (\ProvisioningArtifactProperties' {info} -> info) (\s@ProvisioningArtifactProperties' {} a -> s {info = a} :: ProvisioningArtifactProperties) Prelude.. Lens._Coerce

instance
  Prelude.Hashable
    ProvisioningArtifactProperties

instance
  Prelude.NFData
    ProvisioningArtifactProperties

instance Core.ToJSON ProvisioningArtifactProperties where
  toJSON ProvisioningArtifactProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DisableTemplateValidation" Core..=)
              Prelude.<$> disableTemplateValidation,
            ("Name" Core..=) Prelude.<$> name,
            ("Description" Core..=) Prelude.<$> description,
            ("Type" Core..=) Prelude.<$> type',
            Prelude.Just ("Info" Core..= info)
          ]
      )
