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
-- Module      : Amazonka.CodeDeploy.Types.TagFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TagFilter where

import Amazonka.CodeDeploy.Types.TagFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an on-premises instance tag filter.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | The on-premises instance tag filter key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The on-premises instance tag filter type:
    --
    -- -   KEY_ONLY: Key only.
    --
    -- -   VALUE_ONLY: Value only.
    --
    -- -   KEY_AND_VALUE: Key and value.
    type' :: Prelude.Maybe TagFilterType,
    -- | The on-premises instance tag filter value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagFilter_key' - The on-premises instance tag filter key.
--
-- 'type'', 'tagFilter_type' - The on-premises instance tag filter type:
--
-- -   KEY_ONLY: Key only.
--
-- -   VALUE_ONLY: Value only.
--
-- -   KEY_AND_VALUE: Key and value.
--
-- 'value', 'tagFilter_value' - The on-premises instance tag filter value.
newTagFilter ::
  TagFilter
newTagFilter =
  TagFilter'
    { key = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The on-premises instance tag filter key.
tagFilter_key :: Lens.Lens' TagFilter (Prelude.Maybe Prelude.Text)
tagFilter_key = Lens.lens (\TagFilter' {key} -> key) (\s@TagFilter' {} a -> s {key = a} :: TagFilter)

-- | The on-premises instance tag filter type:
--
-- -   KEY_ONLY: Key only.
--
-- -   VALUE_ONLY: Value only.
--
-- -   KEY_AND_VALUE: Key and value.
tagFilter_type :: Lens.Lens' TagFilter (Prelude.Maybe TagFilterType)
tagFilter_type = Lens.lens (\TagFilter' {type'} -> type') (\s@TagFilter' {} a -> s {type' = a} :: TagFilter)

-- | The on-premises instance tag filter value.
tagFilter_value :: Lens.Lens' TagFilter (Prelude.Maybe Prelude.Text)
tagFilter_value = Lens.lens (\TagFilter' {value} -> value) (\s@TagFilter' {} a -> s {value = a} :: TagFilter)

instance Data.FromJSON TagFilter where
  parseJSON =
    Data.withObject
      "TagFilter"
      ( \x ->
          TagFilter'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable TagFilter where
  hashWithSalt _salt TagFilter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData TagFilter where
  rnf TagFilter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Type" Data..=) Prelude.<$> type',
            ("Value" Data..=) Prelude.<$> value
          ]
      )
