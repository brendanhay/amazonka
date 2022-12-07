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
-- Module      : Amazonka.GuardDuty.Types.ScanConditionPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanConditionPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents key, value pair to be matched against given resource
-- property.
--
-- /See:/ 'newScanConditionPair' smart constructor.
data ScanConditionPair = ScanConditionPair'
  { -- | Represents optional /value/ ____ in the map condition. If not specified,
    -- only /key/ ____ will be matched.
    value :: Prelude.Maybe Prelude.Text,
    -- | Represents /key/ ____ in the map condition.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanConditionPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'scanConditionPair_value' - Represents optional /value/ ____ in the map condition. If not specified,
-- only /key/ ____ will be matched.
--
-- 'key', 'scanConditionPair_key' - Represents /key/ ____ in the map condition.
newScanConditionPair ::
  -- | 'key'
  Prelude.Text ->
  ScanConditionPair
newScanConditionPair pKey_ =
  ScanConditionPair'
    { value = Prelude.Nothing,
      key = pKey_
    }

-- | Represents optional /value/ ____ in the map condition. If not specified,
-- only /key/ ____ will be matched.
scanConditionPair_value :: Lens.Lens' ScanConditionPair (Prelude.Maybe Prelude.Text)
scanConditionPair_value = Lens.lens (\ScanConditionPair' {value} -> value) (\s@ScanConditionPair' {} a -> s {value = a} :: ScanConditionPair)

-- | Represents /key/ ____ in the map condition.
scanConditionPair_key :: Lens.Lens' ScanConditionPair Prelude.Text
scanConditionPair_key = Lens.lens (\ScanConditionPair' {key} -> key) (\s@ScanConditionPair' {} a -> s {key = a} :: ScanConditionPair)

instance Data.FromJSON ScanConditionPair where
  parseJSON =
    Data.withObject
      "ScanConditionPair"
      ( \x ->
          ScanConditionPair'
            Prelude.<$> (x Data..:? "value") Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable ScanConditionPair where
  hashWithSalt _salt ScanConditionPair' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData ScanConditionPair where
  rnf ScanConditionPair' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Data.ToJSON ScanConditionPair where
  toJSON ScanConditionPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("value" Data..=) Prelude.<$> value,
            Prelude.Just ("key" Data..= key)
          ]
      )
