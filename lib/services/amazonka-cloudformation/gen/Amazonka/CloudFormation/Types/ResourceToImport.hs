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
-- Module      : Amazonka.CloudFormation.Types.ResourceToImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ResourceToImport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the target resource of an import operation.
--
-- /See:/ 'newResourceToImport' smart constructor.
data ResourceToImport = ResourceToImport'
  { -- | The type of resource to import into your stack, such as
    -- @AWS::S3::Bucket@. For a list of supported resource types, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations>
    -- in the CloudFormation User Guide.
    resourceType :: Prelude.Text,
    -- | The logical ID of the target resource as specified in the template.
    logicalResourceId :: Prelude.Text,
    -- | A key-value pair that identifies the target resource. The key is an
    -- identifier property (for example, @BucketName@ for @AWS::S3::Bucket@
    -- resources) and the value is the actual property value (for example,
    -- @MyS3Bucket@).
    resourceIdentifier :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceToImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resourceToImport_resourceType' - The type of resource to import into your stack, such as
-- @AWS::S3::Bucket@. For a list of supported resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations>
-- in the CloudFormation User Guide.
--
-- 'logicalResourceId', 'resourceToImport_logicalResourceId' - The logical ID of the target resource as specified in the template.
--
-- 'resourceIdentifier', 'resourceToImport_resourceIdentifier' - A key-value pair that identifies the target resource. The key is an
-- identifier property (for example, @BucketName@ for @AWS::S3::Bucket@
-- resources) and the value is the actual property value (for example,
-- @MyS3Bucket@).
newResourceToImport ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'logicalResourceId'
  Prelude.Text ->
  ResourceToImport
newResourceToImport
  pResourceType_
  pLogicalResourceId_ =
    ResourceToImport'
      { resourceType = pResourceType_,
        logicalResourceId = pLogicalResourceId_,
        resourceIdentifier = Prelude.mempty
      }

-- | The type of resource to import into your stack, such as
-- @AWS::S3::Bucket@. For a list of supported resource types, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations>
-- in the CloudFormation User Guide.
resourceToImport_resourceType :: Lens.Lens' ResourceToImport Prelude.Text
resourceToImport_resourceType = Lens.lens (\ResourceToImport' {resourceType} -> resourceType) (\s@ResourceToImport' {} a -> s {resourceType = a} :: ResourceToImport)

-- | The logical ID of the target resource as specified in the template.
resourceToImport_logicalResourceId :: Lens.Lens' ResourceToImport Prelude.Text
resourceToImport_logicalResourceId = Lens.lens (\ResourceToImport' {logicalResourceId} -> logicalResourceId) (\s@ResourceToImport' {} a -> s {logicalResourceId = a} :: ResourceToImport)

-- | A key-value pair that identifies the target resource. The key is an
-- identifier property (for example, @BucketName@ for @AWS::S3::Bucket@
-- resources) and the value is the actual property value (for example,
-- @MyS3Bucket@).
resourceToImport_resourceIdentifier :: Lens.Lens' ResourceToImport (Prelude.HashMap Prelude.Text Prelude.Text)
resourceToImport_resourceIdentifier = Lens.lens (\ResourceToImport' {resourceIdentifier} -> resourceIdentifier) (\s@ResourceToImport' {} a -> s {resourceIdentifier = a} :: ResourceToImport) Prelude.. Lens.coerced

instance Prelude.Hashable ResourceToImport where
  hashWithSalt _salt ResourceToImport' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData ResourceToImport where
  rnf ResourceToImport' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance Data.ToQuery ResourceToImport where
  toQuery ResourceToImport' {..} =
    Prelude.mconcat
      [ "ResourceType" Data.=: resourceType,
        "LogicalResourceId" Data.=: logicalResourceId,
        "ResourceIdentifier"
          Data.=: Data.toQueryMap
            "entry"
            "key"
            "value"
            resourceIdentifier
      ]
