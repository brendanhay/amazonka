{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigExportDeliveryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigExportDeliveryInfo
  ( ConfigExportDeliveryInfo (..),

    -- * Smart constructor
    mkConfigExportDeliveryInfo,

    -- * Lenses
    cediLastErrorCode,
    cediLastAttemptTime,
    cediLastSuccessfulTime,
    cediLastStatus,
    cediLastErrorMessage,
    cediNextDeliveryTime,
  )
where

import Network.AWS.Config.Types.DeliveryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides status of the delivery of the snapshot or the configuration history to the specified Amazon S3 bucket. Also provides the status of notifications about the Amazon S3 delivery to the specified Amazon SNS topic.
--
-- /See:/ 'mkConfigExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
  { -- | The error code from the last attempted delivery.
    lastErrorCode :: Lude.Maybe Lude.Text,
    -- | The time of the last attempted delivery.
    lastAttemptTime :: Lude.Maybe Lude.Timestamp,
    -- | The time of the last successful delivery.
    lastSuccessfulTime :: Lude.Maybe Lude.Timestamp,
    -- | Status of the last attempted delivery.
    lastStatus :: Lude.Maybe DeliveryStatus,
    -- | The error message from the last attempted delivery.
    lastErrorMessage :: Lude.Maybe Lude.Text,
    -- | The time that the next delivery occurs.
    nextDeliveryTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigExportDeliveryInfo' with the minimum fields required to make a request.
--
-- * 'lastErrorCode' - The error code from the last attempted delivery.
-- * 'lastAttemptTime' - The time of the last attempted delivery.
-- * 'lastSuccessfulTime' - The time of the last successful delivery.
-- * 'lastStatus' - Status of the last attempted delivery.
-- * 'lastErrorMessage' - The error message from the last attempted delivery.
-- * 'nextDeliveryTime' - The time that the next delivery occurs.
mkConfigExportDeliveryInfo ::
  ConfigExportDeliveryInfo
mkConfigExportDeliveryInfo =
  ConfigExportDeliveryInfo'
    { lastErrorCode = Lude.Nothing,
      lastAttemptTime = Lude.Nothing,
      lastSuccessfulTime = Lude.Nothing,
      lastStatus = Lude.Nothing,
      lastErrorMessage = Lude.Nothing,
      nextDeliveryTime = Lude.Nothing
    }

-- | The error code from the last attempted delivery.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastErrorCode :: Lens.Lens' ConfigExportDeliveryInfo (Lude.Maybe Lude.Text)
cediLastErrorCode = Lens.lens (lastErrorCode :: ConfigExportDeliveryInfo -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorCode = a} :: ConfigExportDeliveryInfo)
{-# DEPRECATED cediLastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead." #-}

-- | The time of the last attempted delivery.
--
-- /Note:/ Consider using 'lastAttemptTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastAttemptTime :: Lens.Lens' ConfigExportDeliveryInfo (Lude.Maybe Lude.Timestamp)
cediLastAttemptTime = Lens.lens (lastAttemptTime :: ConfigExportDeliveryInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAttemptTime = a} :: ConfigExportDeliveryInfo)
{-# DEPRECATED cediLastAttemptTime "Use generic-lens or generic-optics with 'lastAttemptTime' instead." #-}

-- | The time of the last successful delivery.
--
-- /Note:/ Consider using 'lastSuccessfulTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastSuccessfulTime :: Lens.Lens' ConfigExportDeliveryInfo (Lude.Maybe Lude.Timestamp)
cediLastSuccessfulTime = Lens.lens (lastSuccessfulTime :: ConfigExportDeliveryInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastSuccessfulTime = a} :: ConfigExportDeliveryInfo)
{-# DEPRECATED cediLastSuccessfulTime "Use generic-lens or generic-optics with 'lastSuccessfulTime' instead." #-}

-- | Status of the last attempted delivery.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastStatus :: Lens.Lens' ConfigExportDeliveryInfo (Lude.Maybe DeliveryStatus)
cediLastStatus = Lens.lens (lastStatus :: ConfigExportDeliveryInfo -> Lude.Maybe DeliveryStatus) (\s a -> s {lastStatus = a} :: ConfigExportDeliveryInfo)
{-# DEPRECATED cediLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The error message from the last attempted delivery.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediLastErrorMessage :: Lens.Lens' ConfigExportDeliveryInfo (Lude.Maybe Lude.Text)
cediLastErrorMessage = Lens.lens (lastErrorMessage :: ConfigExportDeliveryInfo -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorMessage = a} :: ConfigExportDeliveryInfo)
{-# DEPRECATED cediLastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead." #-}

-- | The time that the next delivery occurs.
--
-- /Note:/ Consider using 'nextDeliveryTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cediNextDeliveryTime :: Lens.Lens' ConfigExportDeliveryInfo (Lude.Maybe Lude.Timestamp)
cediNextDeliveryTime = Lens.lens (nextDeliveryTime :: ConfigExportDeliveryInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {nextDeliveryTime = a} :: ConfigExportDeliveryInfo)
{-# DEPRECATED cediNextDeliveryTime "Use generic-lens or generic-optics with 'nextDeliveryTime' instead." #-}

instance Lude.FromJSON ConfigExportDeliveryInfo where
  parseJSON =
    Lude.withObject
      "ConfigExportDeliveryInfo"
      ( \x ->
          ConfigExportDeliveryInfo'
            Lude.<$> (x Lude..:? "lastErrorCode")
            Lude.<*> (x Lude..:? "lastAttemptTime")
            Lude.<*> (x Lude..:? "lastSuccessfulTime")
            Lude.<*> (x Lude..:? "lastStatus")
            Lude.<*> (x Lude..:? "lastErrorMessage")
            Lude.<*> (x Lude..:? "nextDeliveryTime")
      )
