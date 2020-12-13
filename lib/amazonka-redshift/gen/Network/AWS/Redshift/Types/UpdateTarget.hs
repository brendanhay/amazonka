{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UpdateTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UpdateTarget
  ( UpdateTarget (..),

    -- * Smart constructor
    mkUpdateTarget,

    -- * Lenses
    utDatabaseVersion,
    utMaintenanceTrackName,
    utSupportedOperations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SupportedOperation

-- | A maintenance track that you can switch the current track to.
--
-- /See:/ 'mkUpdateTarget' smart constructor.
data UpdateTarget = UpdateTarget'
  { -- | The cluster version for the new maintenance track.
    databaseVersion :: Lude.Maybe Lude.Text,
    -- | The name of the new maintenance track.
    maintenanceTrackName :: Lude.Maybe Lude.Text,
    -- | A list of operations supported by the maintenance track.
    supportedOperations :: Lude.Maybe [SupportedOperation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTarget' with the minimum fields required to make a request.
--
-- * 'databaseVersion' - The cluster version for the new maintenance track.
-- * 'maintenanceTrackName' - The name of the new maintenance track.
-- * 'supportedOperations' - A list of operations supported by the maintenance track.
mkUpdateTarget ::
  UpdateTarget
mkUpdateTarget =
  UpdateTarget'
    { databaseVersion = Lude.Nothing,
      maintenanceTrackName = Lude.Nothing,
      supportedOperations = Lude.Nothing
    }

-- | The cluster version for the new maintenance track.
--
-- /Note:/ Consider using 'databaseVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDatabaseVersion :: Lens.Lens' UpdateTarget (Lude.Maybe Lude.Text)
utDatabaseVersion = Lens.lens (databaseVersion :: UpdateTarget -> Lude.Maybe Lude.Text) (\s a -> s {databaseVersion = a} :: UpdateTarget)
{-# DEPRECATED utDatabaseVersion "Use generic-lens or generic-optics with 'databaseVersion' instead." #-}

-- | The name of the new maintenance track.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utMaintenanceTrackName :: Lens.Lens' UpdateTarget (Lude.Maybe Lude.Text)
utMaintenanceTrackName = Lens.lens (maintenanceTrackName :: UpdateTarget -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: UpdateTarget)
{-# DEPRECATED utMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | A list of operations supported by the maintenance track.
--
-- /Note:/ Consider using 'supportedOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSupportedOperations :: Lens.Lens' UpdateTarget (Lude.Maybe [SupportedOperation])
utSupportedOperations = Lens.lens (supportedOperations :: UpdateTarget -> Lude.Maybe [SupportedOperation]) (\s a -> s {supportedOperations = a} :: UpdateTarget)
{-# DEPRECATED utSupportedOperations "Use generic-lens or generic-optics with 'supportedOperations' instead." #-}

instance Lude.FromXML UpdateTarget where
  parseXML x =
    UpdateTarget'
      Lude.<$> (x Lude..@? "DatabaseVersion")
      Lude.<*> (x Lude..@? "MaintenanceTrackName")
      Lude.<*> ( x Lude..@? "SupportedOperations" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "SupportedOperation")
               )
