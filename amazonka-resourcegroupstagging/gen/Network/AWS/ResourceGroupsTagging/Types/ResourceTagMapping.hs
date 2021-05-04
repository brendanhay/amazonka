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
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
import Network.AWS.ResourceGroupsTagging.Types.Tag

-- | A list of resource ARNs and the tags (keys and values) that are
-- associated with each.
--
-- /See:/ 'newResourceTagMapping' smart constructor.
data ResourceTagMapping = ResourceTagMapping'
  { -- | The ARN of the resource.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | Information that shows whether a resource is compliant with the
    -- effective tag policy, including details on any noncompliant tag keys.
    complianceDetails :: Prelude.Maybe ComplianceDetails,
    -- | The tags that have been applied to one or more AWS resources.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceTagMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'resourceTagMapping_resourceARN' - The ARN of the resource.
--
-- 'complianceDetails', 'resourceTagMapping_complianceDetails' - Information that shows whether a resource is compliant with the
-- effective tag policy, including details on any noncompliant tag keys.
--
-- 'tags', 'resourceTagMapping_tags' - The tags that have been applied to one or more AWS resources.
newResourceTagMapping ::
  ResourceTagMapping
newResourceTagMapping =
  ResourceTagMapping'
    { resourceARN = Prelude.Nothing,
      complianceDetails = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the resource.
resourceTagMapping_resourceARN :: Lens.Lens' ResourceTagMapping (Prelude.Maybe Prelude.Text)
resourceTagMapping_resourceARN = Lens.lens (\ResourceTagMapping' {resourceARN} -> resourceARN) (\s@ResourceTagMapping' {} a -> s {resourceARN = a} :: ResourceTagMapping)

-- | Information that shows whether a resource is compliant with the
-- effective tag policy, including details on any noncompliant tag keys.
resourceTagMapping_complianceDetails :: Lens.Lens' ResourceTagMapping (Prelude.Maybe ComplianceDetails)
resourceTagMapping_complianceDetails = Lens.lens (\ResourceTagMapping' {complianceDetails} -> complianceDetails) (\s@ResourceTagMapping' {} a -> s {complianceDetails = a} :: ResourceTagMapping)

-- | The tags that have been applied to one or more AWS resources.
resourceTagMapping_tags :: Lens.Lens' ResourceTagMapping (Prelude.Maybe [Tag])
resourceTagMapping_tags = Lens.lens (\ResourceTagMapping' {tags} -> tags) (\s@ResourceTagMapping' {} a -> s {tags = a} :: ResourceTagMapping) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ResourceTagMapping where
  parseJSON =
    Prelude.withObject
      "ResourceTagMapping"
      ( \x ->
          ResourceTagMapping'
            Prelude.<$> (x Prelude..:? "ResourceARN")
            Prelude.<*> (x Prelude..:? "ComplianceDetails")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceTagMapping

instance Prelude.NFData ResourceTagMapping
