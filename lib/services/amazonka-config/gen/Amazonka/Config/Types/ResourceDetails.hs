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
-- Module      : Amazonka.Config.Types.ResourceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceDetails where

import Amazonka.Config.Types.ResourceConfigurationSchemaType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the resource being evaluated.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | The schema type of the resource configuration.
    --
    -- You can find the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource type schema>,
    -- or @CFN_RESOURCE_SCHEMA@, in \"/Amazon Web Services public extensions/\"
    -- within the CloudFormation registry or with the following CLI commmand:
    -- @aws cloudformation describe-type --type-name \"AWS::S3::Bucket\" --type RESOURCE@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry.html#registry-view Managing extensions through the CloudFormation registry>
    -- and
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
    -- in the CloudFormation User Guide.
    resourceConfigurationSchemaType :: Prelude.Maybe ResourceConfigurationSchemaType,
    -- | A unique resource ID for an evaluation.
    resourceId :: Prelude.Text,
    -- | The type of resource being evaluated.
    resourceType :: Prelude.Text,
    -- | The resource definition to be evaluated as per the resource
    -- configuration schema type.
    resourceConfiguration :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceConfigurationSchemaType', 'resourceDetails_resourceConfigurationSchemaType' - The schema type of the resource configuration.
--
-- You can find the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource type schema>,
-- or @CFN_RESOURCE_SCHEMA@, in \"/Amazon Web Services public extensions/\"
-- within the CloudFormation registry or with the following CLI commmand:
-- @aws cloudformation describe-type --type-name \"AWS::S3::Bucket\" --type RESOURCE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry.html#registry-view Managing extensions through the CloudFormation registry>
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the CloudFormation User Guide.
--
-- 'resourceId', 'resourceDetails_resourceId' - A unique resource ID for an evaluation.
--
-- 'resourceType', 'resourceDetails_resourceType' - The type of resource being evaluated.
--
-- 'resourceConfiguration', 'resourceDetails_resourceConfiguration' - The resource definition to be evaluated as per the resource
-- configuration schema type.
newResourceDetails ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resourceConfiguration'
  Prelude.Text ->
  ResourceDetails
newResourceDetails
  pResourceId_
  pResourceType_
  pResourceConfiguration_ =
    ResourceDetails'
      { resourceConfigurationSchemaType =
          Prelude.Nothing,
        resourceId = pResourceId_,
        resourceType = pResourceType_,
        resourceConfiguration = pResourceConfiguration_
      }

-- | The schema type of the resource configuration.
--
-- You can find the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource type schema>,
-- or @CFN_RESOURCE_SCHEMA@, in \"/Amazon Web Services public extensions/\"
-- within the CloudFormation registry or with the following CLI commmand:
-- @aws cloudformation describe-type --type-name \"AWS::S3::Bucket\" --type RESOURCE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry.html#registry-view Managing extensions through the CloudFormation registry>
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services resource and property types reference>
-- in the CloudFormation User Guide.
resourceDetails_resourceConfigurationSchemaType :: Lens.Lens' ResourceDetails (Prelude.Maybe ResourceConfigurationSchemaType)
resourceDetails_resourceConfigurationSchemaType = Lens.lens (\ResourceDetails' {resourceConfigurationSchemaType} -> resourceConfigurationSchemaType) (\s@ResourceDetails' {} a -> s {resourceConfigurationSchemaType = a} :: ResourceDetails)

-- | A unique resource ID for an evaluation.
resourceDetails_resourceId :: Lens.Lens' ResourceDetails Prelude.Text
resourceDetails_resourceId = Lens.lens (\ResourceDetails' {resourceId} -> resourceId) (\s@ResourceDetails' {} a -> s {resourceId = a} :: ResourceDetails)

-- | The type of resource being evaluated.
resourceDetails_resourceType :: Lens.Lens' ResourceDetails Prelude.Text
resourceDetails_resourceType = Lens.lens (\ResourceDetails' {resourceType} -> resourceType) (\s@ResourceDetails' {} a -> s {resourceType = a} :: ResourceDetails)

-- | The resource definition to be evaluated as per the resource
-- configuration schema type.
resourceDetails_resourceConfiguration :: Lens.Lens' ResourceDetails Prelude.Text
resourceDetails_resourceConfiguration = Lens.lens (\ResourceDetails' {resourceConfiguration} -> resourceConfiguration) (\s@ResourceDetails' {} a -> s {resourceConfiguration = a} :: ResourceDetails)

instance Data.FromJSON ResourceDetails where
  parseJSON =
    Data.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Data..:? "ResourceConfigurationSchemaType")
            Prelude.<*> (x Data..: "ResourceId")
            Prelude.<*> (x Data..: "ResourceType")
            Prelude.<*> (x Data..: "ResourceConfiguration")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` resourceConfigurationSchemaType
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceConfiguration

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} =
    Prelude.rnf resourceConfigurationSchemaType
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceConfiguration

instance Data.ToJSON ResourceDetails where
  toJSON ResourceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceConfigurationSchemaType" Data..=)
              Prelude.<$> resourceConfigurationSchemaType,
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just
              ( "ResourceConfiguration"
                  Data..= resourceConfiguration
              )
          ]
      )
