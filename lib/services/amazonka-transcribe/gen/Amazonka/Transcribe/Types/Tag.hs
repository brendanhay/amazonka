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
-- Module      : Amazonka.Transcribe.Types.Tag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Adds metadata, in the form of a key:value pair, to the specified
-- resource.
--
-- For example, you could add the tag @Department:Sales@ to a resource to
-- indicate that it pertains to your organization\'s sales department. You
-- can also use tags for tag-based access control.
--
-- To learn more about tagging, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The first part of a key:value pair that forms a tag associated with a
    -- given resource. For example, in the tag @Department:Sales@, the key is
    -- \'Department\'.
    key :: Prelude.Text,
    -- | The second part of a key:value pair that forms a tag associated with a
    -- given resource. For example, in the tag @Department:Sales@, the value is
    -- \'Sales\'.
    --
    -- Note that you can set the value of a tag to an empty string, but you
    -- can\'t set the value of a tag to null. Omitting the tag value is the
    -- same as using an empty string.
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
-- given resource. For example, in the tag @Department:Sales@, the key is
-- \'Department\'.
--
-- 'value', 'tag_value' - The second part of a key:value pair that forms a tag associated with a
-- given resource. For example, in the tag @Department:Sales@, the value is
-- \'Sales\'.
--
-- Note that you can set the value of a tag to an empty string, but you
-- can\'t set the value of a tag to null. Omitting the tag value is the
-- same as using an empty string.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | The first part of a key:value pair that forms a tag associated with a
-- given resource. For example, in the tag @Department:Sales@, the key is
-- \'Department\'.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The second part of a key:value pair that forms a tag associated with a
-- given resource. For example, in the tag @Department:Sales@, the value is
-- \'Sales\'.
--
-- Note that you can set the value of a tag to an empty string, but you
-- can\'t set the value of a tag to null. Omitting the tag value is the
-- same as using an empty string.
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

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Value" Core..= value)
          ]
      )
