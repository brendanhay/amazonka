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
-- Module      : Amazonka.AppMesh.Types.TagRef
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TagRef where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Optional metadata that you apply to a resource to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- /See:/ 'newTagRef' smart constructor.
data TagRef = TagRef'
  { -- | One part of a key-value pair that make up a tag. A @key@ is a general
    -- label that acts like a category for more specific tag values.
    key :: Prelude.Text,
    -- | The optional part of a key-value pair that make up a tag. A @value@ acts
    -- as a descriptor within a tag category (key).
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagRef_key' - One part of a key-value pair that make up a tag. A @key@ is a general
-- label that acts like a category for more specific tag values.
--
-- 'value', 'tagRef_value' - The optional part of a key-value pair that make up a tag. A @value@ acts
-- as a descriptor within a tag category (key).
newTagRef ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  TagRef
newTagRef pKey_ pValue_ =
  TagRef' {key = pKey_, value = pValue_}

-- | One part of a key-value pair that make up a tag. A @key@ is a general
-- label that acts like a category for more specific tag values.
tagRef_key :: Lens.Lens' TagRef Prelude.Text
tagRef_key = Lens.lens (\TagRef' {key} -> key) (\s@TagRef' {} a -> s {key = a} :: TagRef)

-- | The optional part of a key-value pair that make up a tag. A @value@ acts
-- as a descriptor within a tag category (key).
tagRef_value :: Lens.Lens' TagRef Prelude.Text
tagRef_value = Lens.lens (\TagRef' {value} -> value) (\s@TagRef' {} a -> s {value = a} :: TagRef)

instance Core.FromJSON TagRef where
  parseJSON =
    Core.withObject
      "TagRef"
      ( \x ->
          TagRef'
            Prelude.<$> (x Core..: "key") Prelude.<*> (x Core..: "value")
      )

instance Prelude.Hashable TagRef where
  hashWithSalt _salt TagRef' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData TagRef where
  rnf TagRef' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToJSON TagRef where
  toJSON TagRef' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Core..= key),
            Prelude.Just ("value" Core..= value)
          ]
      )
