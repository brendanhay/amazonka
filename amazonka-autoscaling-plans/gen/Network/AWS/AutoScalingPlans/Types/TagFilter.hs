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
-- Module      : Network.AWS.AutoScalingPlans.Types.TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.TagFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a tag.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | The tag key.
    key :: Core.Maybe Core.Text,
    -- | The tag values (0 to 20).
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
-- 'key', 'tagFilter_key' - The tag key.
--
-- 'values', 'tagFilter_values' - The tag values (0 to 20).
newTagFilter ::
  TagFilter
newTagFilter =
  TagFilter'
    { key = Core.Nothing,
      values = Core.Nothing
    }

-- | The tag key.
tagFilter_key :: Lens.Lens' TagFilter (Core.Maybe Core.Text)
tagFilter_key = Lens.lens (\TagFilter' {key} -> key) (\s@TagFilter' {} a -> s {key = a} :: TagFilter)

-- | The tag values (0 to 20).
tagFilter_values :: Lens.Lens' TagFilter (Core.Maybe [Core.Text])
tagFilter_values = Lens.lens (\TagFilter' {values} -> values) (\s@TagFilter' {} a -> s {values = a} :: TagFilter) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON TagFilter where
  parseJSON =
    Core.withObject
      "TagFilter"
      ( \x ->
          TagFilter'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
      )

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
