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
-- Module      : Network.AWS.DAX.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A description of a tag. Every tag is a key-value pair. You can add up to
-- 50 tags to a single DAX cluster.
--
-- AWS-assigned tag names and values are automatically assigned the @aws:@
-- prefix, which the user cannot assign. AWS-assigned tag names do not
-- count towards the tag limit of 50. User-assigned tag names have the
-- prefix @user:@.
--
-- You cannot backdate the application of a tag.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key for the tag. Tag keys are case sensitive. Every DAX cluster can
    -- only have one tag with the same key. If you try to add an existing tag
    -- (same key), the existing tag value will be updated to the new value.
    key :: Core.Maybe Core.Text,
    -- | The value of the tag. Tag values are case-sensitive and can be null.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tag_key' - The key for the tag. Tag keys are case sensitive. Every DAX cluster can
-- only have one tag with the same key. If you try to add an existing tag
-- (same key), the existing tag value will be updated to the new value.
--
-- 'value', 'tag_value' - The value of the tag. Tag values are case-sensitive and can be null.
newTag ::
  Tag
newTag =
  Tag' {key = Core.Nothing, value = Core.Nothing}

-- | The key for the tag. Tag keys are case sensitive. Every DAX cluster can
-- only have one tag with the same key. If you try to add an existing tag
-- (same key), the existing tag value will be updated to the new value.
tag_key :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of the tag. Tag values are case-sensitive and can be null.
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Core.<$> (x Core..:? "Key") Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Value" Core..=) Core.<$> value
          ]
      )
