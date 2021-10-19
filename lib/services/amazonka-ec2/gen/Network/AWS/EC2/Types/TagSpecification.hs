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
-- Module      : Network.AWS.EC2.Types.TagSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TagSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The tags to apply to a resource when the resource is being created.
--
-- /See:/ 'newTagSpecification' smart constructor.
data TagSpecification = TagSpecification'
  { -- | The type of resource to tag on creation.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The tags to apply to the resource.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'tagSpecification_resourceType' - The type of resource to tag on creation.
--
-- 'tags', 'tagSpecification_tags' - The tags to apply to the resource.
newTagSpecification ::
  TagSpecification
newTagSpecification =
  TagSpecification'
    { resourceType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The type of resource to tag on creation.
tagSpecification_resourceType :: Lens.Lens' TagSpecification (Prelude.Maybe ResourceType)
tagSpecification_resourceType = Lens.lens (\TagSpecification' {resourceType} -> resourceType) (\s@TagSpecification' {} a -> s {resourceType = a} :: TagSpecification)

-- | The tags to apply to the resource.
tagSpecification_tags :: Lens.Lens' TagSpecification (Prelude.Maybe [Tag])
tagSpecification_tags = Lens.lens (\TagSpecification' {tags} -> tags) (\s@TagSpecification' {} a -> s {tags = a} :: TagSpecification) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML TagSpecification where
  parseXML x =
    TagSpecification'
      Prelude.<$> (x Core..@? "resourceType")
      Prelude.<*> ( x Core..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable TagSpecification

instance Prelude.NFData TagSpecification

instance Core.ToQuery TagSpecification where
  toQuery TagSpecification' {..} =
    Prelude.mconcat
      [ "ResourceType" Core.=: resourceType,
        Core.toQuery
          (Core.toQueryList "Tag" Prelude.<$> tags)
      ]
