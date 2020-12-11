-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.LogGroupField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.LogGroupField
  ( LogGroupField (..),

    -- * Smart constructor
    mkLogGroupField,

    -- * Lenses
    lgfPercent,
    lgfName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The fields contained in log events found by a @GetLogGroupFields@ operation, along with the percentage of queried log events in which each field appears.
--
-- /See:/ 'mkLogGroupField' smart constructor.
data LogGroupField = LogGroupField'
  { percent ::
      Lude.Maybe Lude.Natural,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogGroupField' with the minimum fields required to make a request.
--
-- * 'name' - The name of a log field.
-- * 'percent' - The percentage of log events queried that contained the field.
mkLogGroupField ::
  LogGroupField
mkLogGroupField =
  LogGroupField' {percent = Lude.Nothing, name = Lude.Nothing}

-- | The percentage of log events queried that contained the field.
--
-- /Note:/ Consider using 'percent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfPercent :: Lens.Lens' LogGroupField (Lude.Maybe Lude.Natural)
lgfPercent = Lens.lens (percent :: LogGroupField -> Lude.Maybe Lude.Natural) (\s a -> s {percent = a} :: LogGroupField)
{-# DEPRECATED lgfPercent "Use generic-lens or generic-optics with 'percent' instead." #-}

-- | The name of a log field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfName :: Lens.Lens' LogGroupField (Lude.Maybe Lude.Text)
lgfName = Lens.lens (name :: LogGroupField -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LogGroupField)
{-# DEPRECATED lgfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON LogGroupField where
  parseJSON =
    Lude.withObject
      "LogGroupField"
      ( \x ->
          LogGroupField'
            Lude.<$> (x Lude..:? "percent") Lude.<*> (x Lude..:? "name")
      )
