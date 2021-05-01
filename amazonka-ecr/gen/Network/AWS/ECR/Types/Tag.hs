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
-- Module      : Network.AWS.ECR.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Tag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata that you apply to a resource to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define. Tag keys can have a maximum character length of 128
-- characters, and tag values can have a maximum length of 256 characters.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | One part of a key-value pair that make up a tag. A @key@ is a general
    -- label that acts like a category for more specific tag values.
    key :: Prelude.Maybe Prelude.Text,
    -- | The optional part of a key-value pair that make up a tag. A @value@ acts
    -- as a descriptor within a tag category (key).
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
-- 'key', 'tag_key' - One part of a key-value pair that make up a tag. A @key@ is a general
-- label that acts like a category for more specific tag values.
--
-- 'value', 'tag_value' - The optional part of a key-value pair that make up a tag. A @value@ acts
-- as a descriptor within a tag category (key).
newTag ::
  Tag
newTag =
  Tag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | One part of a key-value pair that make up a tag. A @key@ is a general
-- label that acts like a category for more specific tag values.
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The optional part of a key-value pair that make up a tag. A @value@ acts
-- as a descriptor within a tag category (key).
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
