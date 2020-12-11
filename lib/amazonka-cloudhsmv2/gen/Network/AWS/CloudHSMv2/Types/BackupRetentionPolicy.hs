-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
  ( BackupRetentionPolicy (..),

    -- * Smart constructor
    mkBackupRetentionPolicy,

    -- * Lenses
    brpValue,
    brpType,
  )
where

import Network.AWS.CloudHSMv2.Types.BackupRetentionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A policy that defines the number of days to retain backups.
--
-- /See:/ 'mkBackupRetentionPolicy' smart constructor.
data BackupRetentionPolicy = BackupRetentionPolicy'
  { value ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe BackupRetentionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackupRetentionPolicy' with the minimum fields required to make a request.
--
-- * 'type'' - The type of backup retention policy. For the @DAYS@ type, the value is the number of days to retain backups.
-- * 'value' - Use a value between 7 - 379.
mkBackupRetentionPolicy ::
  BackupRetentionPolicy
mkBackupRetentionPolicy =
  BackupRetentionPolicy'
    { value = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Use a value between 7 - 379.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brpValue :: Lens.Lens' BackupRetentionPolicy (Lude.Maybe Lude.Text)
brpValue = Lens.lens (value :: BackupRetentionPolicy -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: BackupRetentionPolicy)
{-# DEPRECATED brpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of backup retention policy. For the @DAYS@ type, the value is the number of days to retain backups.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brpType :: Lens.Lens' BackupRetentionPolicy (Lude.Maybe BackupRetentionType)
brpType = Lens.lens (type' :: BackupRetentionPolicy -> Lude.Maybe BackupRetentionType) (\s a -> s {type' = a} :: BackupRetentionPolicy)
{-# DEPRECATED brpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON BackupRetentionPolicy where
  parseJSON =
    Lude.withObject
      "BackupRetentionPolicy"
      ( \x ->
          BackupRetentionPolicy'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON BackupRetentionPolicy where
  toJSON BackupRetentionPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
