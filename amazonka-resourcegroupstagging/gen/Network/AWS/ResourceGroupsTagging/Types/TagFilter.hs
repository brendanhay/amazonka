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
-- Module      : Network.AWS.ResourceGroupsTagging.Types.TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.TagFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of tags (keys and values) that are used to specify the associated
-- resources.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | One part of a key-value pair that makes up a tag. A key is a general
    -- label that acts like a category for more specific tag values.
    key :: Core.Maybe Core.Text,
    -- | One part of a key-value pair that make up a tag. A value acts as a
    -- descriptor within a tag category (key). The value can be empty or null.
    values :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagFilter_key' - One part of a key-value pair that makes up a tag. A key is a general
-- label that acts like a category for more specific tag values.
--
-- 'values', 'tagFilter_values' - One part of a key-value pair that make up a tag. A value acts as a
-- descriptor within a tag category (key). The value can be empty or null.
newTagFilter ::
  TagFilter
newTagFilter =
  TagFilter'
    { key = Core.Nothing,
      values = Core.Nothing
    }

-- | One part of a key-value pair that makes up a tag. A key is a general
-- label that acts like a category for more specific tag values.
tagFilter_key :: Lens.Lens' TagFilter (Core.Maybe Core.Text)
tagFilter_key = Lens.lens (\TagFilter' {key} -> key) (\s@TagFilter' {} a -> s {key = a} :: TagFilter)

-- | One part of a key-value pair that make up a tag. A value acts as a
-- descriptor within a tag category (key). The value can be empty or null.
tagFilter_values :: Lens.Lens' TagFilter (Core.Maybe [Core.Text])
tagFilter_values = Lens.lens (\TagFilter' {values} -> values) (\s@TagFilter' {} a -> s {values = a} :: TagFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable TagFilter

instance Core.NFData TagFilter

instance Core.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values
          ]
      )
