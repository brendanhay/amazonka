{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
  ( PendingModifiedRelationalDatabaseValues (..),

    -- * Smart constructor
    mkPendingModifiedRelationalDatabaseValues,

    -- * Lenses
    pmrdvEngineVersion,
    pmrdvMasterUserPassword,
    pmrdvBackupRetentionEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a pending database value modification.
--
-- /See:/ 'mkPendingModifiedRelationalDatabaseValues' smart constructor.
data PendingModifiedRelationalDatabaseValues = PendingModifiedRelationalDatabaseValues'
  { -- | The database engine version.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | The password for the master user of the database.
    masterUserPassword :: Lude.Maybe Lude.Text,
    -- | A Boolean value indicating whether automated backup retention is enabled.
    backupRetentionEnabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingModifiedRelationalDatabaseValues' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The database engine version.
-- * 'masterUserPassword' - The password for the master user of the database.
-- * 'backupRetentionEnabled' - A Boolean value indicating whether automated backup retention is enabled.
mkPendingModifiedRelationalDatabaseValues ::
  PendingModifiedRelationalDatabaseValues
mkPendingModifiedRelationalDatabaseValues =
  PendingModifiedRelationalDatabaseValues'
    { engineVersion =
        Lude.Nothing,
      masterUserPassword = Lude.Nothing,
      backupRetentionEnabled = Lude.Nothing
    }

-- | The database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvEngineVersion :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Lude.Maybe Lude.Text)
pmrdvEngineVersion = Lens.lens (engineVersion :: PendingModifiedRelationalDatabaseValues -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: PendingModifiedRelationalDatabaseValues)
{-# DEPRECATED pmrdvEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The password for the master user of the database.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvMasterUserPassword :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Lude.Maybe Lude.Text)
pmrdvMasterUserPassword = Lens.lens (masterUserPassword :: PendingModifiedRelationalDatabaseValues -> Lude.Maybe Lude.Text) (\s a -> s {masterUserPassword = a} :: PendingModifiedRelationalDatabaseValues)
{-# DEPRECATED pmrdvMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | A Boolean value indicating whether automated backup retention is enabled.
--
-- /Note:/ Consider using 'backupRetentionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmrdvBackupRetentionEnabled :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Lude.Maybe Lude.Bool)
pmrdvBackupRetentionEnabled = Lens.lens (backupRetentionEnabled :: PendingModifiedRelationalDatabaseValues -> Lude.Maybe Lude.Bool) (\s a -> s {backupRetentionEnabled = a} :: PendingModifiedRelationalDatabaseValues)
{-# DEPRECATED pmrdvBackupRetentionEnabled "Use generic-lens or generic-optics with 'backupRetentionEnabled' instead." #-}

instance Lude.FromJSON PendingModifiedRelationalDatabaseValues where
  parseJSON =
    Lude.withObject
      "PendingModifiedRelationalDatabaseValues"
      ( \x ->
          PendingModifiedRelationalDatabaseValues'
            Lude.<$> (x Lude..:? "engineVersion")
            Lude.<*> (x Lude..:? "masterUserPassword")
            Lude.<*> (x Lude..:? "backupRetentionEnabled")
      )
