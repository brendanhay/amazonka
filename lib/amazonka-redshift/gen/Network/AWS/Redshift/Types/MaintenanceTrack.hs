-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.MaintenanceTrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.MaintenanceTrack
  ( MaintenanceTrack (..),

    -- * Smart constructor
    mkMaintenanceTrack,

    -- * Lenses
    mtDatabaseVersion,
    mtMaintenanceTrackName,
    mtUpdateTargets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.UpdateTarget

-- | Defines a maintenance track that determines which Amazon Redshift version to apply during a maintenance window. If the value for @MaintenanceTrack@ is @current@ , the cluster is updated to the most recently certified maintenance release. If the value is @trailing@ , the cluster is updated to the previously certified maintenance release.
--
-- /See:/ 'mkMaintenanceTrack' smart constructor.
data MaintenanceTrack = MaintenanceTrack'
  { databaseVersion ::
      Lude.Maybe Lude.Text,
    maintenanceTrackName :: Lude.Maybe Lude.Text,
    updateTargets :: Lude.Maybe [UpdateTarget]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceTrack' with the minimum fields required to make a request.
--
-- * 'databaseVersion' - The version number for the cluster release.
-- * 'maintenanceTrackName' - The name of the maintenance track. Possible values are @current@ and @trailing@ .
-- * 'updateTargets' - An array of 'UpdateTarget' objects to update with the maintenance track.
mkMaintenanceTrack ::
  MaintenanceTrack
mkMaintenanceTrack =
  MaintenanceTrack'
    { databaseVersion = Lude.Nothing,
      maintenanceTrackName = Lude.Nothing,
      updateTargets = Lude.Nothing
    }

-- | The version number for the cluster release.
--
-- /Note:/ Consider using 'databaseVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtDatabaseVersion :: Lens.Lens' MaintenanceTrack (Lude.Maybe Lude.Text)
mtDatabaseVersion = Lens.lens (databaseVersion :: MaintenanceTrack -> Lude.Maybe Lude.Text) (\s a -> s {databaseVersion = a} :: MaintenanceTrack)
{-# DEPRECATED mtDatabaseVersion "Use generic-lens or generic-optics with 'databaseVersion' instead." #-}

-- | The name of the maintenance track. Possible values are @current@ and @trailing@ .
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMaintenanceTrackName :: Lens.Lens' MaintenanceTrack (Lude.Maybe Lude.Text)
mtMaintenanceTrackName = Lens.lens (maintenanceTrackName :: MaintenanceTrack -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: MaintenanceTrack)
{-# DEPRECATED mtMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | An array of 'UpdateTarget' objects to update with the maintenance track.
--
-- /Note:/ Consider using 'updateTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtUpdateTargets :: Lens.Lens' MaintenanceTrack (Lude.Maybe [UpdateTarget])
mtUpdateTargets = Lens.lens (updateTargets :: MaintenanceTrack -> Lude.Maybe [UpdateTarget]) (\s a -> s {updateTargets = a} :: MaintenanceTrack)
{-# DEPRECATED mtUpdateTargets "Use generic-lens or generic-optics with 'updateTargets' instead." #-}

instance Lude.FromXML MaintenanceTrack where
  parseXML x =
    MaintenanceTrack'
      Lude.<$> (x Lude..@? "DatabaseVersion")
      Lude.<*> (x Lude..@? "MaintenanceTrackName")
      Lude.<*> ( x Lude..@? "UpdateTargets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "UpdateTarget")
               )
