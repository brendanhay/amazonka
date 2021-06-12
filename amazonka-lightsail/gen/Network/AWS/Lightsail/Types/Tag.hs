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
-- Module      : Network.AWS.Lightsail.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a tag key and optional value assigned to an Amazon Lightsail
-- resource.
--
-- For more information about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key of the tag.
    --
    -- Constraints: Tag keys accept a maximum of 128 letters, numbers, spaces
    -- in UTF-8, or the following characters: + - = . _ : \/ \@
    key :: Core.Maybe Core.Text,
    -- | The value of the tag.
    --
    -- Constraints: Tag values accept a maximum of 256 letters, numbers, spaces
    -- in UTF-8, or the following characters: + - = . _ : \/ \@
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
-- 'key', 'tag_key' - The key of the tag.
--
-- Constraints: Tag keys accept a maximum of 128 letters, numbers, spaces
-- in UTF-8, or the following characters: + - = . _ : \/ \@
--
-- 'value', 'tag_value' - The value of the tag.
--
-- Constraints: Tag values accept a maximum of 256 letters, numbers, spaces
-- in UTF-8, or the following characters: + - = . _ : \/ \@
newTag ::
  Tag
newTag =
  Tag' {key = Core.Nothing, value = Core.Nothing}

-- | The key of the tag.
--
-- Constraints: Tag keys accept a maximum of 128 letters, numbers, spaces
-- in UTF-8, or the following characters: + - = . _ : \/ \@
tag_key :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of the tag.
--
-- Constraints: Tag values accept a maximum of 256 letters, numbers, spaces
-- in UTF-8, or the following characters: + - = . _ : \/ \@
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Core.<$> (x Core..:? "key") Core.<*> (x Core..:? "value")
      )

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Core.catMaybes
          [ ("key" Core..=) Core.<$> key,
            ("value" Core..=) Core.<$> value
          ]
      )
