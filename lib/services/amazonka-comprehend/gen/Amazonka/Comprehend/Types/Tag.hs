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
-- Module      : Amazonka.Comprehend.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair that adds as a metadata to a resource used by Amazon
-- Comprehend. For example, a tag with the key-value pair
-- ‘Department’:’Sales’ might be added to a resource to indicate its use by
-- a particular department.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The second part of a key-value pair that forms a tag associated with a
    -- given resource. For instance, if you want to show which resources are
    -- used by which departments, you might use “Department” as the initial
    -- (key) portion of the pair, with a value of “sales” to indicate the sales
    -- department.
    value :: Prelude.Maybe Prelude.Text,
    -- | The initial part of a key-value pair that forms a tag associated with a
    -- given resource. For instance, if you want to show which resources are
    -- used by which departments, you might use “Department” as the key portion
    -- of the pair, with multiple possible values such as “sales,” “legal,” and
    -- “administration.”
    key :: Prelude.Text
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
-- 'value', 'tag_value' - The second part of a key-value pair that forms a tag associated with a
-- given resource. For instance, if you want to show which resources are
-- used by which departments, you might use “Department” as the initial
-- (key) portion of the pair, with a value of “sales” to indicate the sales
-- department.
--
-- 'key', 'tag_key' - The initial part of a key-value pair that forms a tag associated with a
-- given resource. For instance, if you want to show which resources are
-- used by which departments, you might use “Department” as the key portion
-- of the pair, with multiple possible values such as “sales,” “legal,” and
-- “administration.”
newTag ::
  -- | 'key'
  Prelude.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Prelude.Nothing, key = pKey_}

-- | The second part of a key-value pair that forms a tag associated with a
-- given resource. For instance, if you want to show which resources are
-- used by which departments, you might use “Department” as the initial
-- (key) portion of the pair, with a value of “sales” to indicate the sales
-- department.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | The initial part of a key-value pair that forms a tag associated with a
-- given resource. For instance, if you want to show which resources are
-- used by which departments, you might use “Department” as the key portion
-- of the pair, with multiple possible values such as “sales,” “legal,” and
-- “administration.”
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Data.FromJSON Tag where
  parseJSON =
    Data.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Data..:? "Value")
            Prelude.<*> (x Data..: "Key")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Data.ToJSON Tag where
  toJSON Tag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Key" Data..= key)
          ]
      )
