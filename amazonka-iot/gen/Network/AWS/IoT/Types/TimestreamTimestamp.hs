{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.TimestreamTimestamp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamTimestamp where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON TimestreamTimestamp where
  parseJSON =
    Prelude.withObject
      "TimestreamTimestamp"
      ( \x ->
          TimestreamTimestamp'
            Prelude.<$> (x Prelude..: "value")
            Prelude.<*> (x Prelude..: "unit")
      )

instance Prelude.Hashable TimestreamTimestamp

instance Prelude.NFData TimestreamTimestamp

instance Prelude.ToJSON TimestreamTimestamp where
  toJSON TimestreamTimestamp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("value" Prelude..= value),
            Prelude.Just ("unit" Prelude..= unit)
          ]
      )
