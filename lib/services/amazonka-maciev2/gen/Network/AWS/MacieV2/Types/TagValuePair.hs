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
-- Module      : Network.AWS.MacieV2.Types.TagValuePair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.TagValuePair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a tag key or tag key and value pair to use in a tag-based
-- condition that determines whether an S3 object is included or excluded
-- from a classification job. Tag keys and values are case sensitive. Also,
-- Amazon Macie doesn\'t support use of partial values or wildcard
-- characters in tag-based conditions.
--
-- /See:/ 'newTagValuePair' smart constructor.
data TagValuePair = TagValuePair'
  { -- | The tag value, associated with the specified tag key (key), to use in
    -- the condition. To specify only a tag key for a condition, specify the
    -- tag key for the key property and set this value to an empty string.
    value :: Prelude.Maybe Prelude.Text,
    -- | The value for the tag key to use in the condition.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tagValuePair_value' - The tag value, associated with the specified tag key (key), to use in
-- the condition. To specify only a tag key for a condition, specify the
-- tag key for the key property and set this value to an empty string.
--
-- 'key', 'tagValuePair_key' - The value for the tag key to use in the condition.
newTagValuePair ::
  TagValuePair
newTagValuePair =
  TagValuePair'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The tag value, associated with the specified tag key (key), to use in
-- the condition. To specify only a tag key for a condition, specify the
-- tag key for the key property and set this value to an empty string.
tagValuePair_value :: Lens.Lens' TagValuePair (Prelude.Maybe Prelude.Text)
tagValuePair_value = Lens.lens (\TagValuePair' {value} -> value) (\s@TagValuePair' {} a -> s {value = a} :: TagValuePair)

-- | The value for the tag key to use in the condition.
tagValuePair_key :: Lens.Lens' TagValuePair (Prelude.Maybe Prelude.Text)
tagValuePair_key = Lens.lens (\TagValuePair' {key} -> key) (\s@TagValuePair' {} a -> s {key = a} :: TagValuePair)

instance Core.FromJSON TagValuePair where
  parseJSON =
    Core.withObject
      "TagValuePair"
      ( \x ->
          TagValuePair'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "key")
      )

instance Prelude.Hashable TagValuePair

instance Prelude.NFData TagValuePair

instance Core.ToJSON TagValuePair where
  toJSON TagValuePair' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("key" Core..=) Prelude.<$> key
          ]
      )
