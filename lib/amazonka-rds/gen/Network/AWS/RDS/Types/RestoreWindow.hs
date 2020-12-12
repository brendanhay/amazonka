{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.RestoreWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.RestoreWindow
  ( RestoreWindow (..),

    -- * Smart constructor
    mkRestoreWindow,

    -- * Lenses
    rwLatestTime,
    rwEarliestTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Earliest and latest time an instance can be restored to:
--
-- /See:/ 'mkRestoreWindow' smart constructor.
data RestoreWindow = RestoreWindow'
  { latestTime ::
      Lude.Maybe Lude.DateTime,
    earliestTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreWindow' with the minimum fields required to make a request.
--
-- * 'earliestTime' - The earliest time you can restore an instance to.
-- * 'latestTime' - The latest time you can restore an instance to.
mkRestoreWindow ::
  RestoreWindow
mkRestoreWindow =
  RestoreWindow'
    { latestTime = Lude.Nothing,
      earliestTime = Lude.Nothing
    }

-- | The latest time you can restore an instance to.
--
-- /Note:/ Consider using 'latestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwLatestTime :: Lens.Lens' RestoreWindow (Lude.Maybe Lude.DateTime)
rwLatestTime = Lens.lens (latestTime :: RestoreWindow -> Lude.Maybe Lude.DateTime) (\s a -> s {latestTime = a} :: RestoreWindow)
{-# DEPRECATED rwLatestTime "Use generic-lens or generic-optics with 'latestTime' instead." #-}

-- | The earliest time you can restore an instance to.
--
-- /Note:/ Consider using 'earliestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwEarliestTime :: Lens.Lens' RestoreWindow (Lude.Maybe Lude.DateTime)
rwEarliestTime = Lens.lens (earliestTime :: RestoreWindow -> Lude.Maybe Lude.DateTime) (\s a -> s {earliestTime = a} :: RestoreWindow)
{-# DEPRECATED rwEarliestTime "Use generic-lens or generic-optics with 'earliestTime' instead." #-}

instance Lude.FromXML RestoreWindow where
  parseXML x =
    RestoreWindow'
      Lude.<$> (x Lude..@? "LatestTime") Lude.<*> (x Lude..@? "EarliestTime")
