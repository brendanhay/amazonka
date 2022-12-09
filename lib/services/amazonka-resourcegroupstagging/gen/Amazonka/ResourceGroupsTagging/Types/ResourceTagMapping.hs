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
-- Module      : Amazonka.ResourceGroupsTagging.Types.ResourceTagMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Types.ResourceTagMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroupsTagging.Types.ComplianceDetails
import Amazonka.ResourceGroupsTagging.Types.Tag

-- | A list of resource ARNs and the tags (keys and values) that are
-- associated with each.
--
-- /See:/ 'newResourceTagMapping' smart constructor.
data ResourceTagMapping = ResourceTagMapping'
  { -- | Information that shows whether a resource is compliant with the
    -- effective tag policy, including details on any noncompliant tag keys.
    complianceDetails :: Prelude.Maybe ComplianceDetails,
    -- | The ARN of the resource.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The tags that have been applied to one or more Amazon Web Services
    -- resources.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTagMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceDetails', 'resourceTagMapping_complianceDetails' - Information that shows whether a resource is compliant with the
-- effective tag policy, including details on any noncompliant tag keys.
--
-- 'resourceARN', 'resourceTagMapping_resourceARN' - The ARN of the resource.
--
-- 'tags', 'resourceTagMapping_tags' - The tags that have been applied to one or more Amazon Web Services
-- resources.
newResourceTagMapping ::
  ResourceTagMapping
newResourceTagMapping =
  ResourceTagMapping'
    { complianceDetails =
        Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Information that shows whether a resource is compliant with the
-- effective tag policy, including details on any noncompliant tag keys.
resourceTagMapping_complianceDetails :: Lens.Lens' ResourceTagMapping (Prelude.Maybe ComplianceDetails)
resourceTagMapping_complianceDetails = Lens.lens (\ResourceTagMapping' {complianceDetails} -> complianceDetails) (\s@ResourceTagMapping' {} a -> s {complianceDetails = a} :: ResourceTagMapping)

-- | The ARN of the resource.
resourceTagMapping_resourceARN :: Lens.Lens' ResourceTagMapping (Prelude.Maybe Prelude.Text)
resourceTagMapping_resourceARN = Lens.lens (\ResourceTagMapping' {resourceARN} -> resourceARN) (\s@ResourceTagMapping' {} a -> s {resourceARN = a} :: ResourceTagMapping)

-- | The tags that have been applied to one or more Amazon Web Services
-- resources.
resourceTagMapping_tags :: Lens.Lens' ResourceTagMapping (Prelude.Maybe [Tag])
resourceTagMapping_tags = Lens.lens (\ResourceTagMapping' {tags} -> tags) (\s@ResourceTagMapping' {} a -> s {tags = a} :: ResourceTagMapping) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceTagMapping where
  parseJSON =
    Data.withObject
      "ResourceTagMapping"
      ( \x ->
          ResourceTagMapping'
            Prelude.<$> (x Data..:? "ComplianceDetails")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceTagMapping where
  hashWithSalt _salt ResourceTagMapping' {..} =
    _salt `Prelude.hashWithSalt` complianceDetails
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ResourceTagMapping where
  rnf ResourceTagMapping' {..} =
    Prelude.rnf complianceDetails
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf tags
