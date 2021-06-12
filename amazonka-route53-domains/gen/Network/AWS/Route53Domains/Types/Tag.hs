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
-- Module      : Network.AWS.Route53Domains.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Each tag includes the following elements.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key (name) of a tag.
    --
    -- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
    --
    -- Constraints: Each key can be 1-128 characters long.
    key :: Core.Maybe Core.Text,
    -- | The value of a tag.
    --
    -- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
    --
    -- Constraints: Each value can be 0-256 characters long.
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
-- 'key', 'tag_key' - The key (name) of a tag.
--
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
--
-- Constraints: Each key can be 1-128 characters long.
--
-- 'value', 'tag_value' - The value of a tag.
--
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
--
-- Constraints: Each value can be 0-256 characters long.
newTag ::
  Tag
newTag =
  Tag' {key = Core.Nothing, value = Core.Nothing}

-- | The key (name) of a tag.
--
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
--
-- Constraints: Each key can be 1-128 characters long.
tag_key :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of a tag.
--
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
--
-- Constraints: Each value can be 0-256 characters long.
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
