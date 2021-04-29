{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the tag. Tag values are case-sensitive and can be null.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Tag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key for the tag. Tag keys are case sensitive. Every DAX cluster can
-- only have one tag with the same key. If you try to add an existing tag
-- (same key), the existing tag value will be updated to the new value.
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of the tag. Tag values are case-sensitive and can be null.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Prelude.FromJSON Tag where
  parseJSON =
    Prelude.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Prelude.ToJSON Tag where
  toJSON Tag' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
