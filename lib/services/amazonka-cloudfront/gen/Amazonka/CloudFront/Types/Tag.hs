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
-- Module      : Amazonka.CloudFront.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains @Tag@ key and @Tag@ value.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A string that contains an optional @Tag@ value.
    --
    -- The string length should be between 0 and 256 characters. Valid
    -- characters include @a-z@, @A-Z@, @0-9@, space, and the special
    -- characters @_ - . : \/ = + \@@.
    value :: Prelude.Maybe Prelude.Text,
    -- | A string that contains @Tag@ key.
    --
    -- The string length should be between 1 and 128 characters. Valid
    -- characters include @a-z@, @A-Z@, @0-9@, space, and the special
    -- characters @_ - . : \/ = + \@@.
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
-- 'value', 'tag_value' - A string that contains an optional @Tag@ value.
--
-- The string length should be between 0 and 256 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
--
-- 'key', 'tag_key' - A string that contains @Tag@ key.
--
-- The string length should be between 1 and 128 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
newTag ::
  -- | 'key'
  Prelude.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Prelude.Nothing, key = pKey_}

-- | A string that contains an optional @Tag@ value.
--
-- The string length should be between 0 and 256 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | A string that contains @Tag@ key.
--
-- The string length should be between 1 and 128 characters. Valid
-- characters include @a-z@, @A-Z@, @0-9@, space, and the special
-- characters @_ - . : \/ = + \@@.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Data.FromXML Tag where
  parseXML x =
    Tag'
      Prelude.<$> (x Data..@? "Value") Prelude.<*> (x Data..@ "Key")

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Data.ToXML Tag where
  toXML Tag' {..} =
    Prelude.mconcat
      ["Value" Data.@= value, "Key" Data.@= key]
