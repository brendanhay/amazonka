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
-- Module      : Network.AWS.Firehose.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Metadata that you can assign to a delivery stream, consisting of a
-- key-value pair.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | An optional string, which you can use to describe or define the tag.
    -- Maximum length: 256 characters. Valid characters: Unicode letters,
    -- digits, white space, _ . \/ = + - % \@
    value :: Core.Maybe Core.Text,
    -- | A unique identifier for the tag. Maximum length: 128 characters. Valid
    -- characters: Unicode letters, digits, white space, _ . \/ = + - % \@
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
-- 'value', 'tag_value' - An optional string, which you can use to describe or define the tag.
-- Maximum length: 256 characters. Valid characters: Unicode letters,
-- digits, white space, _ . \/ = + - % \@
--
-- 'key', 'tag_key' - A unique identifier for the tag. Maximum length: 128 characters. Valid
-- characters: Unicode letters, digits, white space, _ . \/ = + - % \@
newTag ::
  -- | 'key'
  Core.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Core.Nothing, key = pKey_}

-- | An optional string, which you can use to describe or define the tag.
-- Maximum length: 256 characters. Valid characters: Unicode letters,
-- digits, white space, _ . \/ = + - % \@
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | A unique identifier for the tag. Maximum length: 128 characters. Valid
-- characters: Unicode letters, digits, white space, _ . \/ = + - % \@
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
