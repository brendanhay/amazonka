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
-- Module      : Network.AWS.DynamoDB.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Tag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a tag. A tag is a key-value pair. You can add up to 50 tags to
-- a single DynamoDB table.
--
-- AWS-assigned tag names and values are automatically assigned the @aws:@
-- prefix, which the user cannot assign. AWS-assigned tag names do not
-- count towards the tag limit of 50. User-assigned tag names have the
-- prefix @user:@ in the Cost Allocation Report. You cannot backdate the
-- application of a tag.
--
-- For an overview on tagging DynamoDB resources, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key of the tag. Tag keys are case sensitive. Each DynamoDB table can
    -- only have up to one tag with the same key. If you try to add an existing
    -- tag (same key), the existing tag value will be updated to the new value.
    key :: Prelude.Text,
    -- | The value of the tag. Tag values are case-sensitive and can be null.
    value :: Prelude.Text
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
-- 'key', 'tag_key' - The key of the tag. Tag keys are case sensitive. Each DynamoDB table can
-- only have up to one tag with the same key. If you try to add an existing
-- tag (same key), the existing tag value will be updated to the new value.
--
-- 'value', 'tag_value' - The value of the tag. Tag values are case-sensitive and can be null.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | The key of the tag. Tag keys are case sensitive. Each DynamoDB table can
-- only have up to one tag with the same key. If you try to add an existing
-- tag (same key), the existing tag value will be updated to the new value.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of the tag. Tag values are case-sensitive and can be null.
tag_value :: Lens.Lens' Tag Prelude.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Prelude.FromJSON Tag where
  parseJSON =
    Prelude.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Prelude..: "Key")
            Prelude.<*> (x Prelude..: "Value")
      )

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Prelude.ToJSON Tag where
  toJSON Tag' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Prelude..= key),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
