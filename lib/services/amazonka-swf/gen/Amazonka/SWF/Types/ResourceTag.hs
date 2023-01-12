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
-- Module      : Amazonka.SWF.Types.ResourceTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ResourceTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Tags are key-value pairs that can be associated with Amazon SWF state
-- machines and activities.
--
-- Tags may only contain unicode letters, digits, whitespace, or these
-- symbols: @_ . : \/ = + - \@@.
--
-- /See:/ 'newResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The value of a tag.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key of a tag.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceTag_value' - The value of a tag.
--
-- 'key', 'resourceTag_key' - The key of a tag.
newResourceTag ::
  -- | 'key'
  Prelude.Text ->
  ResourceTag
newResourceTag pKey_ =
  ResourceTag' {value = Prelude.Nothing, key = pKey_}

-- | The value of a tag.
resourceTag_value :: Lens.Lens' ResourceTag (Prelude.Maybe Prelude.Text)
resourceTag_value = Lens.lens (\ResourceTag' {value} -> value) (\s@ResourceTag' {} a -> s {value = a} :: ResourceTag)

-- | The key of a tag.
resourceTag_key :: Lens.Lens' ResourceTag Prelude.Text
resourceTag_key = Lens.lens (\ResourceTag' {key} -> key) (\s@ResourceTag' {} a -> s {key = a} :: ResourceTag)

instance Data.FromJSON ResourceTag where
  parseJSON =
    Data.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Prelude.<$> (x Data..:? "value") Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable ResourceTag where
  hashWithSalt _salt ResourceTag' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData ResourceTag where
  rnf ResourceTag' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Data.ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("value" Data..=) Prelude.<$> value,
            Prelude.Just ("key" Data..= key)
          ]
      )
