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
-- Module      : Amazonka.EC2.Types.TagSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TagSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResourceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The tags to apply to a resource when the resource is being created.
--
-- The @Valid Values@ lists all the resource types that can be tagged.
-- However, the action you\'re using might not support tagging all of these
-- resource types. If you try to tag a resource type that is unsupported
-- for the action you\'re using, you\'ll get an error.
--
-- /See:/ 'newTagSpecification' smart constructor.
data TagSpecification = TagSpecification'
  { -- | The tags to apply to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The type of resource to tag on creation.
    resourceType :: Prelude.Maybe ResourceType
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
-- 'tags', 'tagSpecification_tags' - The tags to apply to the resource.
--
-- 'resourceType', 'tagSpecification_resourceType' - The type of resource to tag on creation.
newTagSpecification ::
  TagSpecification
newTagSpecification =
  TagSpecification'
    { tags = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The tags to apply to the resource.
tagSpecification_tags :: Lens.Lens' TagSpecification (Prelude.Maybe [Tag])
tagSpecification_tags = Lens.lens (\TagSpecification' {tags} -> tags) (\s@TagSpecification' {} a -> s {tags = a} :: TagSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The type of resource to tag on creation.
tagSpecification_resourceType :: Lens.Lens' TagSpecification (Prelude.Maybe ResourceType)
tagSpecification_resourceType = Lens.lens (\TagSpecification' {resourceType} -> resourceType) (\s@TagSpecification' {} a -> s {resourceType = a} :: TagSpecification)

instance Core.FromXML TagSpecification where
  parseXML x =
    TagSpecification'
      Prelude.<$> ( x Core..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "resourceType")

instance Prelude.Hashable TagSpecification where
  hashWithSalt _salt TagSpecification' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData TagSpecification where
  rnf TagSpecification' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType

instance Core.ToQuery TagSpecification where
  toQuery TagSpecification' {..} =
    Prelude.mconcat
      [ Core.toQuery
          (Core.toQueryList "Tag" Prelude.<$> tags),
        "ResourceType" Core.=: resourceType
      ]
