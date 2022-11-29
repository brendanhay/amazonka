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
-- Module      : Amazonka.IoT.Types.TimestreamTimestamp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TimestreamTimestamp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes how to interpret an application-defined timestamp value from
-- an MQTT message payload and the precision of that value.
--
-- /See:/ 'newTimestreamTimestamp' smart constructor.
data TimestreamTimestamp = TimestreamTimestamp'
  { -- | An expression that returns a long epoch time value.
    value :: Prelude.Text,
    -- | The precision of the timestamp value that results from the expression
    -- described in @value@.
    --
    -- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
    -- @NANOSECONDS@. The default is @MILLISECONDS@.
    unit :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestreamTimestamp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'timestreamTimestamp_value' - An expression that returns a long epoch time value.
--
-- 'unit', 'timestreamTimestamp_unit' - The precision of the timestamp value that results from the expression
-- described in @value@.
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
-- @NANOSECONDS@. The default is @MILLISECONDS@.
newTimestreamTimestamp ::
  -- | 'value'
  Prelude.Text ->
  -- | 'unit'
  Prelude.Text ->
  TimestreamTimestamp
newTimestreamTimestamp pValue_ pUnit_ =
  TimestreamTimestamp'
    { value = pValue_,
      unit = pUnit_
    }

-- | An expression that returns a long epoch time value.
timestreamTimestamp_value :: Lens.Lens' TimestreamTimestamp Prelude.Text
timestreamTimestamp_value = Lens.lens (\TimestreamTimestamp' {value} -> value) (\s@TimestreamTimestamp' {} a -> s {value = a} :: TimestreamTimestamp)

-- | The precision of the timestamp value that results from the expression
-- described in @value@.
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
-- @NANOSECONDS@. The default is @MILLISECONDS@.
timestreamTimestamp_unit :: Lens.Lens' TimestreamTimestamp Prelude.Text
timestreamTimestamp_unit = Lens.lens (\TimestreamTimestamp' {unit} -> unit) (\s@TimestreamTimestamp' {} a -> s {unit = a} :: TimestreamTimestamp)

instance Core.FromJSON TimestreamTimestamp where
  parseJSON =
    Core.withObject
      "TimestreamTimestamp"
      ( \x ->
          TimestreamTimestamp'
            Prelude.<$> (x Core..: "value") Prelude.<*> (x Core..: "unit")
      )

instance Prelude.Hashable TimestreamTimestamp where
  hashWithSalt _salt TimestreamTimestamp' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` unit

instance Prelude.NFData TimestreamTimestamp where
  rnf TimestreamTimestamp' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf unit

instance Core.ToJSON TimestreamTimestamp where
  toJSON TimestreamTimestamp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("value" Core..= value),
            Prelude.Just ("unit" Core..= unit)
          ]
      )
