{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.DestinationBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.DestinationBackup
  ( DestinationBackup (..),

    -- * Smart constructor
    mkDestinationBackup,

    -- * Lenses
    dbSourceCluster,
    dbSourceRegion,
    dbSourceBackup,
    dbCreateTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the backup that will be copied and created by the 'CopyBackupToRegion' operation.
--
-- /See:/ 'mkDestinationBackup' smart constructor.
data DestinationBackup = DestinationBackup'
  { -- | The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
    sourceCluster :: Lude.Maybe Lude.Text,
    -- | The AWS region that contains the source backup from which the new backup was copied.
    sourceRegion :: Lude.Maybe Lude.Text,
    -- | The identifier (ID) of the source backup from which the new backup was copied.
    sourceBackup :: Lude.Maybe Lude.Text,
    -- | The date and time when both the source backup was created.
    createTimestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DestinationBackup' with the minimum fields required to make a request.
--
-- * 'sourceCluster' - The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
-- * 'sourceRegion' - The AWS region that contains the source backup from which the new backup was copied.
-- * 'sourceBackup' - The identifier (ID) of the source backup from which the new backup was copied.
-- * 'createTimestamp' - The date and time when both the source backup was created.
mkDestinationBackup ::
  DestinationBackup
mkDestinationBackup =
  DestinationBackup'
    { sourceCluster = Lude.Nothing,
      sourceRegion = Lude.Nothing,
      sourceBackup = Lude.Nothing,
      createTimestamp = Lude.Nothing
    }

-- | The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSourceCluster :: Lens.Lens' DestinationBackup (Lude.Maybe Lude.Text)
dbSourceCluster = Lens.lens (sourceCluster :: DestinationBackup -> Lude.Maybe Lude.Text) (\s a -> s {sourceCluster = a} :: DestinationBackup)
{-# DEPRECATED dbSourceCluster "Use generic-lens or generic-optics with 'sourceCluster' instead." #-}

-- | The AWS region that contains the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSourceRegion :: Lens.Lens' DestinationBackup (Lude.Maybe Lude.Text)
dbSourceRegion = Lens.lens (sourceRegion :: DestinationBackup -> Lude.Maybe Lude.Text) (\s a -> s {sourceRegion = a} :: DestinationBackup)
{-# DEPRECATED dbSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | The identifier (ID) of the source backup from which the new backup was copied.
--
-- /Note:/ Consider using 'sourceBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSourceBackup :: Lens.Lens' DestinationBackup (Lude.Maybe Lude.Text)
dbSourceBackup = Lens.lens (sourceBackup :: DestinationBackup -> Lude.Maybe Lude.Text) (\s a -> s {sourceBackup = a} :: DestinationBackup)
{-# DEPRECATED dbSourceBackup "Use generic-lens or generic-optics with 'sourceBackup' instead." #-}

-- | The date and time when both the source backup was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbCreateTimestamp :: Lens.Lens' DestinationBackup (Lude.Maybe Lude.Timestamp)
dbCreateTimestamp = Lens.lens (createTimestamp :: DestinationBackup -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTimestamp = a} :: DestinationBackup)
{-# DEPRECATED dbCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

instance Lude.FromJSON DestinationBackup where
  parseJSON =
    Lude.withObject
      "DestinationBackup"
      ( \x ->
          DestinationBackup'
            Lude.<$> (x Lude..:? "SourceCluster")
            Lude.<*> (x Lude..:? "SourceRegion")
            Lude.<*> (x Lude..:? "SourceBackup")
            Lude.<*> (x Lude..:? "CreateTimestamp")
      )
