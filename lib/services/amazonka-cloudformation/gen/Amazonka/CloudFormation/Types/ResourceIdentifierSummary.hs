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
-- Module      : Amazonka.CloudFormation.Types.ResourceIdentifierSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ResourceIdentifierSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the target resources of a specific type in your import
-- template (for example, all @AWS::S3::Bucket@ resources) and the
-- properties you can provide during the import to identify resources of
-- that type.
--
-- /See:/ 'newResourceIdentifierSummary' smart constructor.
data ResourceIdentifierSummary = ResourceIdentifierSummary'
  { -- | The template resource type of the target resources, such as
    -- @AWS::S3::Bucket@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The resource properties you can provide during the import to identify
    -- your target resources. For example, @BucketName@ is a possible
    -- identifier property for @AWS::S3::Bucket@ resources.
    resourceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The logical IDs of the target resources of the specified @ResourceType@,
    -- as defined in the import template.
    logicalResourceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceIdentifierSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resourceIdentifierSummary_resourceType' - The template resource type of the target resources, such as
-- @AWS::S3::Bucket@.
--
-- 'resourceIdentifiers', 'resourceIdentifierSummary_resourceIdentifiers' - The resource properties you can provide during the import to identify
-- your target resources. For example, @BucketName@ is a possible
-- identifier property for @AWS::S3::Bucket@ resources.
--
-- 'logicalResourceIds', 'resourceIdentifierSummary_logicalResourceIds' - The logical IDs of the target resources of the specified @ResourceType@,
-- as defined in the import template.
newResourceIdentifierSummary ::
  ResourceIdentifierSummary
newResourceIdentifierSummary =
  ResourceIdentifierSummary'
    { resourceType =
        Prelude.Nothing,
      resourceIdentifiers = Prelude.Nothing,
      logicalResourceIds = Prelude.Nothing
    }

-- | The template resource type of the target resources, such as
-- @AWS::S3::Bucket@.
resourceIdentifierSummary_resourceType :: Lens.Lens' ResourceIdentifierSummary (Prelude.Maybe Prelude.Text)
resourceIdentifierSummary_resourceType = Lens.lens (\ResourceIdentifierSummary' {resourceType} -> resourceType) (\s@ResourceIdentifierSummary' {} a -> s {resourceType = a} :: ResourceIdentifierSummary)

-- | The resource properties you can provide during the import to identify
-- your target resources. For example, @BucketName@ is a possible
-- identifier property for @AWS::S3::Bucket@ resources.
resourceIdentifierSummary_resourceIdentifiers :: Lens.Lens' ResourceIdentifierSummary (Prelude.Maybe [Prelude.Text])
resourceIdentifierSummary_resourceIdentifiers = Lens.lens (\ResourceIdentifierSummary' {resourceIdentifiers} -> resourceIdentifiers) (\s@ResourceIdentifierSummary' {} a -> s {resourceIdentifiers = a} :: ResourceIdentifierSummary) Prelude.. Lens.mapping Lens.coerced

-- | The logical IDs of the target resources of the specified @ResourceType@,
-- as defined in the import template.
resourceIdentifierSummary_logicalResourceIds :: Lens.Lens' ResourceIdentifierSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
resourceIdentifierSummary_logicalResourceIds = Lens.lens (\ResourceIdentifierSummary' {logicalResourceIds} -> logicalResourceIds) (\s@ResourceIdentifierSummary' {} a -> s {logicalResourceIds = a} :: ResourceIdentifierSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ResourceIdentifierSummary where
  parseXML x =
    ResourceIdentifierSummary'
      Prelude.<$> (x Data..@? "ResourceType")
      Prelude.<*> ( x Data..@? "ResourceIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "LogicalResourceIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "member")
                  )

instance Prelude.Hashable ResourceIdentifierSummary where
  hashWithSalt _salt ResourceIdentifierSummary' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceIdentifiers
      `Prelude.hashWithSalt` logicalResourceIds

instance Prelude.NFData ResourceIdentifierSummary where
  rnf ResourceIdentifierSummary' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceIdentifiers
      `Prelude.seq` Prelude.rnf logicalResourceIds
