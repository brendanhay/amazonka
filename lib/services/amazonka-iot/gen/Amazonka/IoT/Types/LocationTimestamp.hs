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
-- Module      : Amazonka.IoT.Types.LocationTimestamp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.LocationTimestamp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes how to interpret an application-defined timestamp value from
-- an MQTT message payload and the precision of that value.
--
-- /See:/ 'newLocationTimestamp' smart constructor.
data LocationTimestamp = LocationTimestamp'
  { -- | The precision of the timestamp value that results from the expression
    -- described in @value@.
    --
    -- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
    -- @NANOSECONDS@. The default is @MILLISECONDS@.
    unit :: Prelude.Maybe Prelude.Text,
    -- | An expression that returns a long epoch time value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationTimestamp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'locationTimestamp_unit' - The precision of the timestamp value that results from the expression
-- described in @value@.
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
-- @NANOSECONDS@. The default is @MILLISECONDS@.
--
-- 'value', 'locationTimestamp_value' - An expression that returns a long epoch time value.
newLocationTimestamp ::
  -- | 'value'
  Prelude.Text ->
  LocationTimestamp
newLocationTimestamp pValue_ =
  LocationTimestamp'
    { unit = Prelude.Nothing,
      value = pValue_
    }

-- | The precision of the timestamp value that results from the expression
-- described in @value@.
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
-- @NANOSECONDS@. The default is @MILLISECONDS@.
locationTimestamp_unit :: Lens.Lens' LocationTimestamp (Prelude.Maybe Prelude.Text)
locationTimestamp_unit = Lens.lens (\LocationTimestamp' {unit} -> unit) (\s@LocationTimestamp' {} a -> s {unit = a} :: LocationTimestamp)

-- | An expression that returns a long epoch time value.
locationTimestamp_value :: Lens.Lens' LocationTimestamp Prelude.Text
locationTimestamp_value = Lens.lens (\LocationTimestamp' {value} -> value) (\s@LocationTimestamp' {} a -> s {value = a} :: LocationTimestamp)

instance Core.FromJSON LocationTimestamp where
  parseJSON =
    Core.withObject
      "LocationTimestamp"
      ( \x ->
          LocationTimestamp'
            Prelude.<$> (x Core..:? "unit") Prelude.<*> (x Core..: "value")
      )

instance Prelude.Hashable LocationTimestamp where
  hashWithSalt _salt LocationTimestamp' {..} =
    _salt `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData LocationTimestamp where
  rnf LocationTimestamp' {..} =
    Prelude.rnf unit `Prelude.seq` Prelude.rnf value

instance Core.ToJSON LocationTimestamp where
  toJSON LocationTimestamp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("unit" Core..=) Prelude.<$> unit,
            Prelude.Just ("value" Core..= value)
          ]
      )
