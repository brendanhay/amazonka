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
-- Module      : Amazonka.ServiceQuotas.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex data type that contains a tag key and tag value.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A string that contains a tag key. The string length should be between 1
    -- and 128 characters. Valid characters include a-z, A-Z, 0-9, space, and
    -- the special characters _ - . : \/ = + \@.
    key :: Prelude.Text,
    -- | A string that contains an optional tag value. The string length should
    -- be between 0 and 256 characters. Valid characters include a-z, A-Z, 0-9,
    -- space, and the special characters _ - . : \/ = + \@.
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
-- 'key', 'tag_key' - A string that contains a tag key. The string length should be between 1
-- and 128 characters. Valid characters include a-z, A-Z, 0-9, space, and
-- the special characters _ - . : \/ = + \@.
--
-- 'value', 'tag_value' - A string that contains an optional tag value. The string length should
-- be between 0 and 256 characters. Valid characters include a-z, A-Z, 0-9,
-- space, and the special characters _ - . : \/ = + \@.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | A string that contains a tag key. The string length should be between 1
-- and 128 characters. Valid characters include a-z, A-Z, 0-9, space, and
-- the special characters _ - . : \/ = + \@.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | A string that contains an optional tag value. The string length should
-- be between 0 and 256 characters. Valid characters include a-z, A-Z, 0-9,
-- space, and the special characters _ - . : \/ = + \@.
tag_value :: Lens.Lens' Tag Prelude.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Data.FromJSON Tag where
  parseJSON =
    Data.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Data..: "Key")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Tag where
  toJSON Tag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
