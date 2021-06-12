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
-- Module      : Network.AWS.LexModels.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of key\/value pairs that identify a bot, bot alias, or bot
-- channel. Tag keys and values can consist of Unicode letters, digits,
-- white space, and any of the following symbols: _ . : \/ = + - \@.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key for the tag. Keys are not case-sensitive and must be unique.
    key :: Core.Text,
    -- | The value associated with a key. The value may be an empty string but it
    -- can\'t be null.
    value :: Core.Text
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
-- 'key', 'tag_key' - The key for the tag. Keys are not case-sensitive and must be unique.
--
-- 'value', 'tag_value' - The value associated with a key. The value may be an empty string but it
-- can\'t be null.
newTag ::
  -- | 'key'
  Core.Text ->
  -- | 'value'
  Core.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | The key for the tag. Keys are not case-sensitive and must be unique.
tag_key :: Lens.Lens' Tag Core.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value associated with a key. The value may be an empty string but it
-- can\'t be null.
tag_value :: Lens.Lens' Tag Core.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Core.<$> (x Core..: "key") Core.<*> (x Core..: "value")
      )

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("key" Core..= key),
            Core.Just ("value" Core..= value)
          ]
      )
