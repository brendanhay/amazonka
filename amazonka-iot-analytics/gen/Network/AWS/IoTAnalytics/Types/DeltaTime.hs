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
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeltaTime where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    offsetSeconds :: Prelude.Int,
    -- | An expression by which the time of the message data might be determined.
    -- This can be the name of a timestamp field or a SQL expression that is
    -- used to derive the time the message data was generated.
    timeExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'timeExpression'
  Prelude.Text ->
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
deltaTime_offsetSeconds :: Lens.Lens' DeltaTime Prelude.Int
deltaTime_offsetSeconds = Lens.lens (\DeltaTime' {offsetSeconds} -> offsetSeconds) (\s@DeltaTime' {} a -> s {offsetSeconds = a} :: DeltaTime)

-- | An expression by which the time of the message data might be determined.
-- This can be the name of a timestamp field or a SQL expression that is
-- used to derive the time the message data was generated.
deltaTime_timeExpression :: Lens.Lens' DeltaTime Prelude.Text
deltaTime_timeExpression = Lens.lens (\DeltaTime' {timeExpression} -> timeExpression) (\s@DeltaTime' {} a -> s {timeExpression = a} :: DeltaTime)

instance Prelude.FromJSON DeltaTime where
  parseJSON =
    Prelude.withObject
      "DeltaTime"
      ( \x ->
          DeltaTime'
            Prelude.<$> (x Prelude..: "offsetSeconds")
            Prelude.<*> (x Prelude..: "timeExpression")
      )

instance Prelude.Hashable DeltaTime

instance Prelude.NFData DeltaTime

instance Prelude.ToJSON DeltaTime where
  toJSON DeltaTime' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("offsetSeconds" Prelude..= offsetSeconds),
            Prelude.Just
              ("timeExpression" Prelude..= timeExpression)
          ]
      )
