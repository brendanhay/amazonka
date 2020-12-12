{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamTimestamp
  ( TimestreamTimestamp (..),

    -- * Smart constructor
    mkTimestreamTimestamp,

    -- * Lenses
    ttValue,
    ttUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes how to interpret an application-defined timestamp value from an MQTT message payload and the precision of that value.
--
-- /See:/ 'mkTimestreamTimestamp' smart constructor.
data TimestreamTimestamp = TimestreamTimestamp'
  { value :: Lude.Text,
    unit :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimestreamTimestamp' with the minimum fields required to make a request.
--
-- * 'unit' - The precision of the timestamp value that results from the expression described in @value@ .
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ | @NANOSECONDS@ . The default is @MILLISECONDS@ .
-- * 'value' - An expression that returns a long epoch time value.
mkTimestreamTimestamp ::
  -- | 'value'
  Lude.Text ->
  -- | 'unit'
  Lude.Text ->
  TimestreamTimestamp
mkTimestreamTimestamp pValue_ pUnit_ =
  TimestreamTimestamp' {value = pValue_, unit = pUnit_}

-- | An expression that returns a long epoch time value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttValue :: Lens.Lens' TimestreamTimestamp Lude.Text
ttValue = Lens.lens (value :: TimestreamTimestamp -> Lude.Text) (\s a -> s {value = a} :: TimestreamTimestamp)
{-# DEPRECATED ttValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The precision of the timestamp value that results from the expression described in @value@ .
--
-- Valid values: @SECONDS@ | @MILLISECONDS@ | @MICROSECONDS@ | @NANOSECONDS@ . The default is @MILLISECONDS@ .
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttUnit :: Lens.Lens' TimestreamTimestamp Lude.Text
ttUnit = Lens.lens (unit :: TimestreamTimestamp -> Lude.Text) (\s a -> s {unit = a} :: TimestreamTimestamp)
{-# DEPRECATED ttUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON TimestreamTimestamp where
  parseJSON =
    Lude.withObject
      "TimestreamTimestamp"
      ( \x ->
          TimestreamTimestamp'
            Lude.<$> (x Lude..: "value") Lude.<*> (x Lude..: "unit")
      )

instance Lude.ToJSON TimestreamTimestamp where
  toJSON TimestreamTimestamp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("value" Lude..= value),
            Lude.Just ("unit" Lude..= unit)
          ]
      )
