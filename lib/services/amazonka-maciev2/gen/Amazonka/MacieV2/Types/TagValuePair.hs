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
-- Module      : Amazonka.MacieV2.Types.TagValuePair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.TagValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a tag key or tag key and value pair to use in a tag-based
-- condition that determines whether an S3 object is included or excluded
-- from a classification job. Tag keys and values are case sensitive. Also,
-- Amazon Macie doesn\'t support use of partial values or wildcard
-- characters in tag-based conditions.
--
-- /See:/ 'newTagValuePair' smart constructor.
data TagValuePair = TagValuePair'
  { -- | The value for the tag key to use in the condition.
    key :: Prelude.Maybe Prelude.Text,
    -- | The tag value, associated with the specified tag key (key), to use in
    -- the condition. To specify only a tag key for a condition, specify the
    -- tag key for the key property and set this value to an empty string.
    value :: Prelude.Maybe Prelude.Text
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
-- 'key', 'tagValuePair_key' - The value for the tag key to use in the condition.
--
-- 'value', 'tagValuePair_value' - The tag value, associated with the specified tag key (key), to use in
-- the condition. To specify only a tag key for a condition, specify the
-- tag key for the key property and set this value to an empty string.
newTagValuePair ::
  TagValuePair
newTagValuePair =
  TagValuePair'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The value for the tag key to use in the condition.
tagValuePair_key :: Lens.Lens' TagValuePair (Prelude.Maybe Prelude.Text)
tagValuePair_key = Lens.lens (\TagValuePair' {key} -> key) (\s@TagValuePair' {} a -> s {key = a} :: TagValuePair)

-- | The tag value, associated with the specified tag key (key), to use in
-- the condition. To specify only a tag key for a condition, specify the
-- tag key for the key property and set this value to an empty string.
tagValuePair_value :: Lens.Lens' TagValuePair (Prelude.Maybe Prelude.Text)
tagValuePair_value = Lens.lens (\TagValuePair' {value} -> value) (\s@TagValuePair' {} a -> s {value = a} :: TagValuePair)

instance Data.FromJSON TagValuePair where
  parseJSON =
    Data.withObject
      "TagValuePair"
      ( \x ->
          TagValuePair'
            Prelude.<$> (x Data..:? "key")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable TagValuePair where
  hashWithSalt _salt TagValuePair' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData TagValuePair where
  rnf TagValuePair' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON TagValuePair where
  toJSON TagValuePair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("value" Data..=) Prelude.<$> value
          ]
      )
