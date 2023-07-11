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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProvisioningArtifactType

-- | Information about a provisioning artifact (also known as a version) for
-- a product.
--
-- /See:/ 'newProvisioningArtifactProperties' smart constructor.
data ProvisioningArtifactProperties = ProvisioningArtifactProperties'
  { -- | The description of the provisioning artifact, including how it differs
    -- from the previous provisioning artifact.
    description :: Prelude.Maybe Prelude.Text,
    -- | If set to true, Service Catalog stops validating the specified
    -- provisioning artifact even if it is invalid.
    disableTemplateValidation :: Prelude.Maybe Prelude.Bool,
    -- | Specify the template source with one of the following options, but not
    -- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ]
    --
    -- The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
    -- format. Specify the URL in JSON format as follows:
    --
    -- @\"LoadTemplateFromURL\": \"https:\/\/s3.amazonaws.com\/cf-templates-ozkq9d3hgiq2-us-east-1\/...\"@
    --
    -- @ImportFromPhysicalId@: The physical id of the resource that contains
    -- the template. Currently only supports CloudFormation stack arn. Specify
    -- the physical id in JSON format as follows:
    -- @ImportFromPhysicalId: “arn:aws:cloudformation:[us-east-1]:[accountId]:stack\/[StackName]\/[resourceId]@
    info :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the provisioning artifact (for example, v1 v2beta). No
    -- spaces are allowed.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of provisioning artifact.
    --
    -- -   @CLOUD_FORMATION_TEMPLATE@ - CloudFormation template
    --
    -- -   @MARKETPLACE_AMI@ - Amazon Web Services Marketplace AMI
    --
    -- -   @MARKETPLACE_CAR@ - Amazon Web Services Marketplace Clusters and
    --     Amazon Web Services Resources
    type' :: Prelude.Maybe ProvisioningArtifactType
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
-- 'description', 'provisioningArtifactProperties_description' - The description of the provisioning artifact, including how it differs
-- from the previous provisioning artifact.
--
-- 'disableTemplateValidation', 'provisioningArtifactProperties_disableTemplateValidation' - If set to true, Service Catalog stops validating the specified
-- provisioning artifact even if it is invalid.
--
-- 'info', 'provisioningArtifactProperties_info' - Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ]
--
-- The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
-- format. Specify the URL in JSON format as follows:
--
-- @\"LoadTemplateFromURL\": \"https:\/\/s3.amazonaws.com\/cf-templates-ozkq9d3hgiq2-us-east-1\/...\"@
--
-- @ImportFromPhysicalId@: The physical id of the resource that contains
-- the template. Currently only supports CloudFormation stack arn. Specify
-- the physical id in JSON format as follows:
-- @ImportFromPhysicalId: “arn:aws:cloudformation:[us-east-1]:[accountId]:stack\/[StackName]\/[resourceId]@
--
-- 'name', 'provisioningArtifactProperties_name' - The name of the provisioning artifact (for example, v1 v2beta). No
-- spaces are allowed.
--
-- 'type'', 'provisioningArtifactProperties_type' - The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - Amazon Web Services Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - Amazon Web Services Marketplace Clusters and
--     Amazon Web Services Resources
newProvisioningArtifactProperties ::
  ProvisioningArtifactProperties
newProvisioningArtifactProperties =
  ProvisioningArtifactProperties'
    { description =
        Prelude.Nothing,
      disableTemplateValidation = Prelude.Nothing,
      info = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The description of the provisioning artifact, including how it differs
-- from the previous provisioning artifact.
provisioningArtifactProperties_description :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe Prelude.Text)
provisioningArtifactProperties_description = Lens.lens (\ProvisioningArtifactProperties' {description} -> description) (\s@ProvisioningArtifactProperties' {} a -> s {description = a} :: ProvisioningArtifactProperties)

-- | If set to true, Service Catalog stops validating the specified
-- provisioning artifact even if it is invalid.
provisioningArtifactProperties_disableTemplateValidation :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe Prelude.Bool)
provisioningArtifactProperties_disableTemplateValidation = Lens.lens (\ProvisioningArtifactProperties' {disableTemplateValidation} -> disableTemplateValidation) (\s@ProvisioningArtifactProperties' {} a -> s {disableTemplateValidation = a} :: ProvisioningArtifactProperties)

-- | Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ]
--
-- The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
-- format. Specify the URL in JSON format as follows:
--
-- @\"LoadTemplateFromURL\": \"https:\/\/s3.amazonaws.com\/cf-templates-ozkq9d3hgiq2-us-east-1\/...\"@
--
-- @ImportFromPhysicalId@: The physical id of the resource that contains
-- the template. Currently only supports CloudFormation stack arn. Specify
-- the physical id in JSON format as follows:
-- @ImportFromPhysicalId: “arn:aws:cloudformation:[us-east-1]:[accountId]:stack\/[StackName]\/[resourceId]@
provisioningArtifactProperties_info :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
provisioningArtifactProperties_info = Lens.lens (\ProvisioningArtifactProperties' {info} -> info) (\s@ProvisioningArtifactProperties' {} a -> s {info = a} :: ProvisioningArtifactProperties) Prelude.. Lens.mapping Lens.coerced

-- | The name of the provisioning artifact (for example, v1 v2beta). No
-- spaces are allowed.
provisioningArtifactProperties_name :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe Prelude.Text)
provisioningArtifactProperties_name = Lens.lens (\ProvisioningArtifactProperties' {name} -> name) (\s@ProvisioningArtifactProperties' {} a -> s {name = a} :: ProvisioningArtifactProperties)

-- | The type of provisioning artifact.
--
-- -   @CLOUD_FORMATION_TEMPLATE@ - CloudFormation template
--
-- -   @MARKETPLACE_AMI@ - Amazon Web Services Marketplace AMI
--
-- -   @MARKETPLACE_CAR@ - Amazon Web Services Marketplace Clusters and
--     Amazon Web Services Resources
provisioningArtifactProperties_type :: Lens.Lens' ProvisioningArtifactProperties (Prelude.Maybe ProvisioningArtifactType)
provisioningArtifactProperties_type = Lens.lens (\ProvisioningArtifactProperties' {type'} -> type') (\s@ProvisioningArtifactProperties' {} a -> s {type' = a} :: ProvisioningArtifactProperties)

instance
  Prelude.Hashable
    ProvisioningArtifactProperties
  where
  hashWithSalt
    _salt
    ProvisioningArtifactProperties' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` disableTemplateValidation
        `Prelude.hashWithSalt` info
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    ProvisioningArtifactProperties
  where
  rnf ProvisioningArtifactProperties' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf disableTemplateValidation
      `Prelude.seq` Prelude.rnf info
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ProvisioningArtifactProperties where
  toJSON ProvisioningArtifactProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DisableTemplateValidation" Data..=)
              Prelude.<$> disableTemplateValidation,
            ("Info" Data..=) Prelude.<$> info,
            ("Name" Data..=) Prelude.<$> name,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
