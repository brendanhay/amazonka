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
-- Module      : Network.AWS.CloudFront.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that contains @Tag@ key and @Tag@ value.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A string that contains an optional @Tag@ value.
    --
    -- The string length should be between 0 and 256 characters. Valid
    -- characters include @a-z@, @A-Z@, @0-9@, space, and the special
    -- characters @_ - . : \/ = + \@@.
    value :: Core.Maybe Core.Text,
    -- | A string that contains @Tag@ key.
    --
    -- The string length should be between 1 and 128 characters. Valid
    -- characters include @a-z@, @A-Z@, @0-9@, space, and the special
    -- characters @_ - . : \/ = + \@@.
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
-- 'value', 'tag_value' - A string that contains an optional @Tag@ value.
--
-- The string length should be between 0 and 256 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
--
-- 'key', 'tag_key' - A string that contains @Tag@ key.
--
-- The string length should be between 1 and 128 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
newTag ::
  -- | 'key'
  Core.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Core.Nothing, key = pKey_}

-- | A string that contains an optional @Tag@ value.
--
-- The string length should be between 0 and 256 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | A string that contains @Tag@ key.
--
-- The string length should be between 1 and 128 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
tag_key :: Lens.Lens' Tag Core.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Core.FromXML Tag where
  parseXML x =
    Tag'
      Core.<$> (x Core..@? "Value") Core.<*> (x Core..@ "Key")

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToXML Tag where
  toXML Tag' {..} =
    Core.mconcat
      ["Value" Core.@= value, "Key" Core.@= key]
