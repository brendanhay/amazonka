{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupDescription
  ( BackupDescription (..),

    -- * Smart constructor
    mkBackupDescription,

    -- * Lenses
    bdBackupDetails,
    bdSourceTableDetails,
    bdSourceTableFeatureDetails,
  )
where

import Network.AWS.DynamoDB.Types.BackupDetails
import Network.AWS.DynamoDB.Types.SourceTableDetails
import Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the description of the backup created for the table.
--
-- /See:/ 'mkBackupDescription' smart constructor.
data BackupDescription = BackupDescription'
  { -- | Contains the details of the backup created for the table.
    backupDetails :: Lude.Maybe BackupDetails,
    -- | Contains the details of the table when the backup was created.
    sourceTableDetails :: Lude.Maybe SourceTableDetails,
    -- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
    sourceTableFeatureDetails :: Lude.Maybe SourceTableFeatureDetails
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackupDescription' with the minimum fields required to make a request.
--
-- * 'backupDetails' - Contains the details of the backup created for the table.
-- * 'sourceTableDetails' - Contains the details of the table when the backup was created.
-- * 'sourceTableFeatureDetails' - Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
mkBackupDescription ::
  BackupDescription
mkBackupDescription =
  BackupDescription'
    { backupDetails = Lude.Nothing,
      sourceTableDetails = Lude.Nothing,
      sourceTableFeatureDetails = Lude.Nothing
    }

-- | Contains the details of the backup created for the table.
--
-- /Note:/ Consider using 'backupDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBackupDetails :: Lens.Lens' BackupDescription (Lude.Maybe BackupDetails)
bdBackupDetails = Lens.lens (backupDetails :: BackupDescription -> Lude.Maybe BackupDetails) (\s a -> s {backupDetails = a} :: BackupDescription)
{-# DEPRECATED bdBackupDetails "Use generic-lens or generic-optics with 'backupDetails' instead." #-}

-- | Contains the details of the table when the backup was created.
--
-- /Note:/ Consider using 'sourceTableDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdSourceTableDetails :: Lens.Lens' BackupDescription (Lude.Maybe SourceTableDetails)
bdSourceTableDetails = Lens.lens (sourceTableDetails :: BackupDescription -> Lude.Maybe SourceTableDetails) (\s a -> s {sourceTableDetails = a} :: BackupDescription)
{-# DEPRECATED bdSourceTableDetails "Use generic-lens or generic-optics with 'sourceTableDetails' instead." #-}

-- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
--
-- /Note:/ Consider using 'sourceTableFeatureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdSourceTableFeatureDetails :: Lens.Lens' BackupDescription (Lude.Maybe SourceTableFeatureDetails)
bdSourceTableFeatureDetails = Lens.lens (sourceTableFeatureDetails :: BackupDescription -> Lude.Maybe SourceTableFeatureDetails) (\s a -> s {sourceTableFeatureDetails = a} :: BackupDescription)
{-# DEPRECATED bdSourceTableFeatureDetails "Use generic-lens or generic-optics with 'sourceTableFeatureDetails' instead." #-}

instance Lude.FromJSON BackupDescription where
  parseJSON =
    Lude.withObject
      "BackupDescription"
      ( \x ->
          BackupDescription'
            Lude.<$> (x Lude..:? "BackupDetails")
            Lude.<*> (x Lude..:? "SourceTableDetails")
            Lude.<*> (x Lude..:? "SourceTableFeatureDetails")
      )
