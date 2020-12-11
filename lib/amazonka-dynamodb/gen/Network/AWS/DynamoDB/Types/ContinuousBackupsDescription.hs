-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
  ( ContinuousBackupsDescription (..),

    -- * Smart constructor
    mkContinuousBackupsDescription,

    -- * Lenses
    cbdPointInTimeRecoveryDescription,
    cbdContinuousBackupsStatus,
  )
where

import Network.AWS.DynamoDB.Types.ContinuousBackupsStatus
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the continuous backups and point in time recovery settings on the table.
--
-- /See:/ 'mkContinuousBackupsDescription' smart constructor.
data ContinuousBackupsDescription = ContinuousBackupsDescription'
  { pointInTimeRecoveryDescription ::
      Lude.Maybe
        PointInTimeRecoveryDescription,
    continuousBackupsStatus ::
      ContinuousBackupsStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinuousBackupsDescription' with the minimum fields required to make a request.
--
-- * 'continuousBackupsStatus' - @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
-- * 'pointInTimeRecoveryDescription' - The description of the point in time recovery settings applied to the table.
mkContinuousBackupsDescription ::
  -- | 'continuousBackupsStatus'
  ContinuousBackupsStatus ->
  ContinuousBackupsDescription
mkContinuousBackupsDescription pContinuousBackupsStatus_ =
  ContinuousBackupsDescription'
    { pointInTimeRecoveryDescription =
        Lude.Nothing,
      continuousBackupsStatus = pContinuousBackupsStatus_
    }

-- | The description of the point in time recovery settings applied to the table.
--
-- /Note:/ Consider using 'pointInTimeRecoveryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbdPointInTimeRecoveryDescription :: Lens.Lens' ContinuousBackupsDescription (Lude.Maybe PointInTimeRecoveryDescription)
cbdPointInTimeRecoveryDescription = Lens.lens (pointInTimeRecoveryDescription :: ContinuousBackupsDescription -> Lude.Maybe PointInTimeRecoveryDescription) (\s a -> s {pointInTimeRecoveryDescription = a} :: ContinuousBackupsDescription)
{-# DEPRECATED cbdPointInTimeRecoveryDescription "Use generic-lens or generic-optics with 'pointInTimeRecoveryDescription' instead." #-}

-- | @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
--
-- /Note:/ Consider using 'continuousBackupsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbdContinuousBackupsStatus :: Lens.Lens' ContinuousBackupsDescription ContinuousBackupsStatus
cbdContinuousBackupsStatus = Lens.lens (continuousBackupsStatus :: ContinuousBackupsDescription -> ContinuousBackupsStatus) (\s a -> s {continuousBackupsStatus = a} :: ContinuousBackupsDescription)
{-# DEPRECATED cbdContinuousBackupsStatus "Use generic-lens or generic-optics with 'continuousBackupsStatus' instead." #-}

instance Lude.FromJSON ContinuousBackupsDescription where
  parseJSON =
    Lude.withObject
      "ContinuousBackupsDescription"
      ( \x ->
          ContinuousBackupsDescription'
            Lude.<$> (x Lude..:? "PointInTimeRecoveryDescription")
            Lude.<*> (x Lude..: "ContinuousBackupsStatus")
      )
