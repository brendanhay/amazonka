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
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeltaTime where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Used to limit data to that which has arrived since the last execution of
-- the action.
--
-- /See:/ 'newDeltaTime' smart constructor.
data DeltaTime = DeltaTime'
  { -- | The number of seconds of estimated in-flight lag time of message data.
    -- When you create dataset contents using message data from a specified
    -- timeframe, some message data might still be in flight when processing
    -- begins, and so do not arrive in time to be processed. Use this field to
    -- make allowances for the in flight time of your message data, so that
    -- data not processed from a previous timeframe is included with the next
    -- timeframe. Otherwise, missed message data would be excluded from
    -- processing during the next timeframe too, because its timestamp places
    -- it within the previous timeframe.
    offsetSeconds :: Core.Int,
    -- | An expression by which the time of the message data might be determined.
    -- This can be the name of a timestamp field or a SQL expression that is
    -- used to derive the time the message data was generated.
    timeExpression :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeltaTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offsetSeconds', 'deltaTime_offsetSeconds' - The number of seconds of estimated in-flight lag time of message data.
-- When you create dataset contents using message data from a specified
-- timeframe, some message data might still be in flight when processing
-- begins, and so do not arrive in time to be processed. Use this field to
-- make allowances for the in flight time of your message data, so that
-- data not processed from a previous timeframe is included with the next
-- timeframe. Otherwise, missed message data would be excluded from
-- processing during the next timeframe too, because its timestamp places
-- it within the previous timeframe.
--
-- 'timeExpression', 'deltaTime_timeExpression' - An expression by which the time of the message data might be determined.
-- This can be the name of a timestamp field or a SQL expression that is
-- used to derive the time the message data was generated.
newDeltaTime ::
  -- | 'offsetSeconds'
  Core.Int ->
  -- | 'timeExpression'
  Core.Text ->
  DeltaTime
newDeltaTime pOffsetSeconds_ pTimeExpression_ =
  DeltaTime'
    { offsetSeconds = pOffsetSeconds_,
      timeExpression = pTimeExpression_
    }

-- | The number of seconds of estimated in-flight lag time of message data.
-- When you create dataset contents using message data from a specified
-- timeframe, some message data might still be in flight when processing
-- begins, and so do not arrive in time to be processed. Use this field to
-- make allowances for the in flight time of your message data, so that
-- data not processed from a previous timeframe is included with the next
-- timeframe. Otherwise, missed message data would be excluded from
-- processing during the next timeframe too, because its timestamp places
-- it within the previous timeframe.
deltaTime_offsetSeconds :: Lens.Lens' DeltaTime Core.Int
deltaTime_offsetSeconds = Lens.lens (\DeltaTime' {offsetSeconds} -> offsetSeconds) (\s@DeltaTime' {} a -> s {offsetSeconds = a} :: DeltaTime)

-- | An expression by which the time of the message data might be determined.
-- This can be the name of a timestamp field or a SQL expression that is
-- used to derive the time the message data was generated.
deltaTime_timeExpression :: Lens.Lens' DeltaTime Core.Text
deltaTime_timeExpression = Lens.lens (\DeltaTime' {timeExpression} -> timeExpression) (\s@DeltaTime' {} a -> s {timeExpression = a} :: DeltaTime)

instance Core.FromJSON DeltaTime where
  parseJSON =
    Core.withObject
      "DeltaTime"
      ( \x ->
          DeltaTime'
            Core.<$> (x Core..: "offsetSeconds")
            Core.<*> (x Core..: "timeExpression")
      )

instance Core.Hashable DeltaTime

instance Core.NFData DeltaTime

instance Core.ToJSON DeltaTime where
  toJSON DeltaTime' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("offsetSeconds" Core..= offsetSeconds),
            Core.Just ("timeExpression" Core..= timeExpression)
          ]
      )
