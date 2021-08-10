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
import qualified Network.AWS.Prelude as Prelude

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
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the tag.
    --
    -- Constraints: Tag values accept a maximum of 256 letters, numbers, spaces
    -- in UTF-8, or the following characters: + - = . _ : \/ \@
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Tag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key of the tag.
--
-- Constraints: Tag keys accept a maximum of 128 letters, numbers, spaces
-- in UTF-8, or the following characters: + - = . _ : \/ \@
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of the tag.
--
-- Constraints: Tag values accept a maximum of 256 letters, numbers, spaces
-- in UTF-8, or the following characters: + - = . _ : \/ \@
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Core..:? "key") Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("key" Core..=) Prelude.<$> key,
            ("value" Core..=) Prelude.<$> value
          ]
      )
