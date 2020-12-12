{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigStreamDeliveryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigStreamDeliveryInfo
  ( ConfigStreamDeliveryInfo (..),

    -- * Smart constructor
    mkConfigStreamDeliveryInfo,

    -- * Lenses
    csdiLastErrorCode,
    csdiLastStatusChangeTime,
    csdiLastStatus,
    csdiLastErrorMessage,
  )
where

import Network.AWS.Config.Types.DeliveryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list that contains the status of the delivery of the configuration stream notification to the Amazon SNS topic.
--
-- /See:/ 'mkConfigStreamDeliveryInfo' smart constructor.
data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo'
  { lastErrorCode ::
      Lude.Maybe Lude.Text,
    lastStatusChangeTime ::
      Lude.Maybe Lude.Timestamp,
    lastStatus :: Lude.Maybe DeliveryStatus,
    lastErrorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigStreamDeliveryInfo' with the minimum fields required to make a request.
--
-- * 'lastErrorCode' - The error code from the last attempted delivery.
-- * 'lastErrorMessage' - The error message from the last attempted delivery.
-- * 'lastStatus' - Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel> for AWS Config is optional. If the SNS delivery is turned off, the last status will be __Not_Applicable__ .
-- * 'lastStatusChangeTime' - The time from the last status change.
mkConfigStreamDeliveryInfo ::
  ConfigStreamDeliveryInfo
mkConfigStreamDeliveryInfo =
  ConfigStreamDeliveryInfo'
    { lastErrorCode = Lude.Nothing,
      lastStatusChangeTime = Lude.Nothing,
      lastStatus = Lude.Nothing,
      lastErrorMessage = Lude.Nothing
    }

-- | The error code from the last attempted delivery.
--
-- /Note:/ Consider using 'lastErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdiLastErrorCode :: Lens.Lens' ConfigStreamDeliveryInfo (Lude.Maybe Lude.Text)
csdiLastErrorCode = Lens.lens (lastErrorCode :: ConfigStreamDeliveryInfo -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorCode = a} :: ConfigStreamDeliveryInfo)
{-# DEPRECATED csdiLastErrorCode "Use generic-lens or generic-optics with 'lastErrorCode' instead." #-}

-- | The time from the last status change.
--
-- /Note:/ Consider using 'lastStatusChangeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdiLastStatusChangeTime :: Lens.Lens' ConfigStreamDeliveryInfo (Lude.Maybe Lude.Timestamp)
csdiLastStatusChangeTime = Lens.lens (lastStatusChangeTime :: ConfigStreamDeliveryInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStatusChangeTime = a} :: ConfigStreamDeliveryInfo)
{-# DEPRECATED csdiLastStatusChangeTime "Use generic-lens or generic-optics with 'lastStatusChangeTime' instead." #-}

-- | Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a <https://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel> for AWS Config is optional. If the SNS delivery is turned off, the last status will be __Not_Applicable__ .
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdiLastStatus :: Lens.Lens' ConfigStreamDeliveryInfo (Lude.Maybe DeliveryStatus)
csdiLastStatus = Lens.lens (lastStatus :: ConfigStreamDeliveryInfo -> Lude.Maybe DeliveryStatus) (\s a -> s {lastStatus = a} :: ConfigStreamDeliveryInfo)
{-# DEPRECATED csdiLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The error message from the last attempted delivery.
--
-- /Note:/ Consider using 'lastErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdiLastErrorMessage :: Lens.Lens' ConfigStreamDeliveryInfo (Lude.Maybe Lude.Text)
csdiLastErrorMessage = Lens.lens (lastErrorMessage :: ConfigStreamDeliveryInfo -> Lude.Maybe Lude.Text) (\s a -> s {lastErrorMessage = a} :: ConfigStreamDeliveryInfo)
{-# DEPRECATED csdiLastErrorMessage "Use generic-lens or generic-optics with 'lastErrorMessage' instead." #-}

instance Lude.FromJSON ConfigStreamDeliveryInfo where
  parseJSON =
    Lude.withObject
      "ConfigStreamDeliveryInfo"
      ( \x ->
          ConfigStreamDeliveryInfo'
            Lude.<$> (x Lude..:? "lastErrorCode")
            Lude.<*> (x Lude..:? "lastStatusChangeTime")
            Lude.<*> (x Lude..:? "lastStatus")
            Lude.<*> (x Lude..:? "lastErrorMessage")
      )
