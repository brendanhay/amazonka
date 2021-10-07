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
-- Module      : Network.AWS.Transcribe.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key:value pair that adds metadata to a resource used by Amazon
-- Transcribe. For example, a tag with the key:value pair
-- ‘Department’:’Sales’ might be added to a resource to indicate its use by
-- your organization\'s sales department.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The first part of a key:value pair that forms a tag associated with a
    -- given resource. For example, in the tag ‘Department’:’Sales’, the key is
    -- \'Department\'.
    key :: Prelude.Text,
    -- | The second part of a key:value pair that forms a tag associated with a
    -- given resource. For example, in the tag ‘Department’:’Sales’, the value
    -- is \'Sales\'.
    value :: Prelude.Text
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
-- 'key', 'tag_key' - The first part of a key:value pair that forms a tag associated with a
-- given resource. For example, in the tag ‘Department’:’Sales’, the key is
-- \'Department\'.
--
-- 'value', 'tag_value' - The second part of a key:value pair that forms a tag associated with a
-- given resource. For example, in the tag ‘Department’:’Sales’, the value
-- is \'Sales\'.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | The first part of a key:value pair that forms a tag associated with a
-- given resource. For example, in the tag ‘Department’:’Sales’, the key is
-- \'Department\'.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The second part of a key:value pair that forms a tag associated with a
-- given resource. For example, in the tag ‘Department’:’Sales’, the value
-- is \'Sales\'.
tag_value :: Lens.Lens' Tag Prelude.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Core..: "Key") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Value" Core..= value)
          ]
      )
