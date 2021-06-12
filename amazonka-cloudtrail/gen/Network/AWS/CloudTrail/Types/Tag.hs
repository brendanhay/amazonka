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
-- Module      : Network.AWS.CloudTrail.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A custom key-value pair associated with a resource such as a CloudTrail
-- trail.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The value in a key-value pair of a tag. The value must be no longer than
    -- 256 Unicode characters.
    value :: Core.Maybe Core.Text,
    -- | The key in a key-value pair. The key must be must be no longer than 128
    -- Unicode characters. The key must be unique for the resource to which it
    -- applies.
    key :: Core.Text
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
-- 'value', 'tag_value' - The value in a key-value pair of a tag. The value must be no longer than
-- 256 Unicode characters.
--
-- 'key', 'tag_key' - The key in a key-value pair. The key must be must be no longer than 128
-- Unicode characters. The key must be unique for the resource to which it
-- applies.
newTag ::
  -- | 'key'
  Core.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Core.Nothing, key = pKey_}

-- | The value in a key-value pair of a tag. The value must be no longer than
-- 256 Unicode characters.
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | The key in a key-value pair. The key must be must be no longer than 128
-- Unicode characters. The key must be unique for the resource to which it
-- applies.
tag_key :: Lens.Lens' Tag Core.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Core.<$> (x Core..:? "Value") Core.<*> (x Core..: "Key")
      )

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Value" Core..=) Core.<$> value,
            Core.Just ("Key" Core..= key)
          ]
      )
