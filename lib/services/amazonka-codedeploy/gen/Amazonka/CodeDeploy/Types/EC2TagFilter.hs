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
-- Module      : Amazonka.CodeDeploy.Types.EC2TagFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.EC2TagFilter where

import Amazonka.CodeDeploy.Types.EC2TagFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an EC2 tag filter.
--
-- /See:/ 'newEC2TagFilter' smart constructor.
data EC2TagFilter = EC2TagFilter'
  { -- | The tag filter key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The tag filter type:
    --
    -- -   @KEY_ONLY@: Key only.
    --
    -- -   @VALUE_ONLY@: Value only.
    --
    -- -   @KEY_AND_VALUE@: Key and value.
    type' :: Prelude.Maybe EC2TagFilterType,
    -- | The tag filter value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'eC2TagFilter_key' - The tag filter key.
--
-- 'type'', 'eC2TagFilter_type' - The tag filter type:
--
-- -   @KEY_ONLY@: Key only.
--
-- -   @VALUE_ONLY@: Value only.
--
-- -   @KEY_AND_VALUE@: Key and value.
--
-- 'value', 'eC2TagFilter_value' - The tag filter value.
newEC2TagFilter ::
  EC2TagFilter
newEC2TagFilter =
  EC2TagFilter'
    { key = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The tag filter key.
eC2TagFilter_key :: Lens.Lens' EC2TagFilter (Prelude.Maybe Prelude.Text)
eC2TagFilter_key = Lens.lens (\EC2TagFilter' {key} -> key) (\s@EC2TagFilter' {} a -> s {key = a} :: EC2TagFilter)

-- | The tag filter type:
--
-- -   @KEY_ONLY@: Key only.
--
-- -   @VALUE_ONLY@: Value only.
--
-- -   @KEY_AND_VALUE@: Key and value.
eC2TagFilter_type :: Lens.Lens' EC2TagFilter (Prelude.Maybe EC2TagFilterType)
eC2TagFilter_type = Lens.lens (\EC2TagFilter' {type'} -> type') (\s@EC2TagFilter' {} a -> s {type' = a} :: EC2TagFilter)

-- | The tag filter value.
eC2TagFilter_value :: Lens.Lens' EC2TagFilter (Prelude.Maybe Prelude.Text)
eC2TagFilter_value = Lens.lens (\EC2TagFilter' {value} -> value) (\s@EC2TagFilter' {} a -> s {value = a} :: EC2TagFilter)

instance Data.FromJSON EC2TagFilter where
  parseJSON =
    Data.withObject
      "EC2TagFilter"
      ( \x ->
          EC2TagFilter'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable EC2TagFilter where
  hashWithSalt _salt EC2TagFilter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData EC2TagFilter where
  rnf EC2TagFilter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON EC2TagFilter where
  toJSON EC2TagFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Type" Data..=) Prelude.<$> type',
            ("Value" Data..=) Prelude.<$> value
          ]
      )
