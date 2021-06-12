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
-- Module      : Network.AWS.Comprehend.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    value :: Core.Maybe Core.Text,
    -- | The initial part of a key-value pair that forms a tag associated with a
    -- given resource. For instance, if you want to show which resources are
    -- used by which departments, you might use “Department” as the key portion
    -- of the pair, with multiple possible values such as “sales,” “legal,” and
    -- “administration.”
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
  Core.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Core.Nothing, key = pKey_}

-- | The second part of a key-value pair that forms a tag associated with a
-- given resource. For instance, if you want to show which resources are
-- used by which departments, you might use “Department” as the initial
-- (key) portion of the pair, with a value of “sales” to indicate the sales
-- department.
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | The initial part of a key-value pair that forms a tag associated with a
-- given resource. For instance, if you want to show which resources are
-- used by which departments, you might use “Department” as the key portion
-- of the pair, with multiple possible values such as “sales,” “legal,” and
-- “administration.”
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
