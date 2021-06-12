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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes how to interpret an application-defined timestamp value from
-- an MQTT message payload and the precision of that value.
--
-- /See:/ 'newTimestreamTimestamp' smart constructor.
data TimestreamTimestamp = TimestreamTimestamp'
  { -- | An expression that returns a long epoch time value.
    value :: Core.Text,
    -- | The precision of the timestamp value that results from the expression
    -- described in @value@.
    --
    -- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
    -- @NANOSECONDS@. The default is @MILLISECONDS@.
    unit :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'unit'
  Core.Text ->
  TimestreamTimestamp
newTimestreamTimestamp pValue_ pUnit_ =
  TimestreamTimestamp'
    { value = pValue_,
      unit = pUnit_
    }

-- | An expression that returns a long epoch time value.
timestreamTimestamp_value :: Lens.Lens' TimestreamTimestamp Core.Text
timestreamTimestamp_value = Lens.lens (\TimestreamTimestamp' {value} -> value) (\s@TimestreamTimestamp' {} a -> s {value = a} :: TimestreamTimestamp)

-- | The precision of the timestamp value that results from the expression
-- described in @value@.
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ |
-- @NANOSECONDS@. The default is @MILLISECONDS@.
timestreamTimestamp_unit :: Lens.Lens' TimestreamTimestamp Core.Text
timestreamTimestamp_unit = Lens.lens (\TimestreamTimestamp' {unit} -> unit) (\s@TimestreamTimestamp' {} a -> s {unit = a} :: TimestreamTimestamp)

instance Core.FromJSON TimestreamTimestamp where
  parseJSON =
    Core.withObject
      "TimestreamTimestamp"
      ( \x ->
          TimestreamTimestamp'
            Core.<$> (x Core..: "value") Core.<*> (x Core..: "unit")
      )

instance Core.Hashable TimestreamTimestamp

instance Core.NFData TimestreamTimestamp

instance Core.ToJSON TimestreamTimestamp where
  toJSON TimestreamTimestamp' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("value" Core..= value),
            Core.Just ("unit" Core..= unit)
          ]
      )
