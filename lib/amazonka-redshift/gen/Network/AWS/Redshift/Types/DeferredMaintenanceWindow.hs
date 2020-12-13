{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DeferredMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DeferredMaintenanceWindow
  ( DeferredMaintenanceWindow (..),

    -- * Smart constructor
    mkDeferredMaintenanceWindow,

    -- * Lenses
    dmwDeferMaintenanceEndTime,
    dmwDeferMaintenanceStartTime,
    dmwDeferMaintenanceIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a deferred maintenance window
--
-- /See:/ 'mkDeferredMaintenanceWindow' smart constructor.
data DeferredMaintenanceWindow = DeferredMaintenanceWindow'
  { -- | A timestamp for the end of the time period when we defer maintenance.
    deferMaintenanceEndTime :: Lude.Maybe Lude.DateTime,
    -- | A timestamp for the beginning of the time period when we defer maintenance.
    deferMaintenanceStartTime :: Lude.Maybe Lude.DateTime,
    -- | A unique identifier for the maintenance window.
    deferMaintenanceIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeferredMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'deferMaintenanceEndTime' - A timestamp for the end of the time period when we defer maintenance.
-- * 'deferMaintenanceStartTime' - A timestamp for the beginning of the time period when we defer maintenance.
-- * 'deferMaintenanceIdentifier' - A unique identifier for the maintenance window.
mkDeferredMaintenanceWindow ::
  DeferredMaintenanceWindow
mkDeferredMaintenanceWindow =
  DeferredMaintenanceWindow'
    { deferMaintenanceEndTime =
        Lude.Nothing,
      deferMaintenanceStartTime = Lude.Nothing,
      deferMaintenanceIdentifier = Lude.Nothing
    }

-- | A timestamp for the end of the time period when we defer maintenance.
--
-- /Note:/ Consider using 'deferMaintenanceEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwDeferMaintenanceEndTime :: Lens.Lens' DeferredMaintenanceWindow (Lude.Maybe Lude.DateTime)
dmwDeferMaintenanceEndTime = Lens.lens (deferMaintenanceEndTime :: DeferredMaintenanceWindow -> Lude.Maybe Lude.DateTime) (\s a -> s {deferMaintenanceEndTime = a} :: DeferredMaintenanceWindow)
{-# DEPRECATED dmwDeferMaintenanceEndTime "Use generic-lens or generic-optics with 'deferMaintenanceEndTime' instead." #-}

-- | A timestamp for the beginning of the time period when we defer maintenance.
--
-- /Note:/ Consider using 'deferMaintenanceStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwDeferMaintenanceStartTime :: Lens.Lens' DeferredMaintenanceWindow (Lude.Maybe Lude.DateTime)
dmwDeferMaintenanceStartTime = Lens.lens (deferMaintenanceStartTime :: DeferredMaintenanceWindow -> Lude.Maybe Lude.DateTime) (\s a -> s {deferMaintenanceStartTime = a} :: DeferredMaintenanceWindow)
{-# DEPRECATED dmwDeferMaintenanceStartTime "Use generic-lens or generic-optics with 'deferMaintenanceStartTime' instead." #-}

-- | A unique identifier for the maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwDeferMaintenanceIdentifier :: Lens.Lens' DeferredMaintenanceWindow (Lude.Maybe Lude.Text)
dmwDeferMaintenanceIdentifier = Lens.lens (deferMaintenanceIdentifier :: DeferredMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {deferMaintenanceIdentifier = a} :: DeferredMaintenanceWindow)
{-# DEPRECATED dmwDeferMaintenanceIdentifier "Use generic-lens or generic-optics with 'deferMaintenanceIdentifier' instead." #-}

instance Lude.FromXML DeferredMaintenanceWindow where
  parseXML x =
    DeferredMaintenanceWindow'
      Lude.<$> (x Lude..@? "DeferMaintenanceEndTime")
      Lude.<*> (x Lude..@? "DeferMaintenanceStartTime")
      Lude.<*> (x Lude..@? "DeferMaintenanceIdentifier")
