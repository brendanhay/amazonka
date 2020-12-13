{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
  ( TransferringInputDeviceSummary (..),

    -- * Smart constructor
    mkTransferringInputDeviceSummary,

    -- * Lenses
    tidsTransferType,
    tidsId,
    tidsTargetCustomerId,
    tidsMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceTransferType
import qualified Network.AWS.Prelude as Lude

-- | Details about the input device that is being transferred.
--
-- /See:/ 'mkTransferringInputDeviceSummary' smart constructor.
data TransferringInputDeviceSummary = TransferringInputDeviceSummary'
  { -- | The type (direction) of the input device transfer.
    transferType :: Lude.Maybe InputDeviceTransferType,
    -- | The unique ID of the input device.
    id :: Lude.Maybe Lude.Text,
    -- | The AWS account ID for the recipient of the input device transfer.
    targetCustomerId :: Lude.Maybe Lude.Text,
    -- | The optional message that the sender has attached to the transfer.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferringInputDeviceSummary' with the minimum fields required to make a request.
--
-- * 'transferType' - The type (direction) of the input device transfer.
-- * 'id' - The unique ID of the input device.
-- * 'targetCustomerId' - The AWS account ID for the recipient of the input device transfer.
-- * 'message' - The optional message that the sender has attached to the transfer.
mkTransferringInputDeviceSummary ::
  TransferringInputDeviceSummary
mkTransferringInputDeviceSummary =
  TransferringInputDeviceSummary'
    { transferType = Lude.Nothing,
      id = Lude.Nothing,
      targetCustomerId = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The type (direction) of the input device transfer.
--
-- /Note:/ Consider using 'transferType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsTransferType :: Lens.Lens' TransferringInputDeviceSummary (Lude.Maybe InputDeviceTransferType)
tidsTransferType = Lens.lens (transferType :: TransferringInputDeviceSummary -> Lude.Maybe InputDeviceTransferType) (\s a -> s {transferType = a} :: TransferringInputDeviceSummary)
{-# DEPRECATED tidsTransferType "Use generic-lens or generic-optics with 'transferType' instead." #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsId :: Lens.Lens' TransferringInputDeviceSummary (Lude.Maybe Lude.Text)
tidsId = Lens.lens (id :: TransferringInputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: TransferringInputDeviceSummary)
{-# DEPRECATED tidsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The AWS account ID for the recipient of the input device transfer.
--
-- /Note:/ Consider using 'targetCustomerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsTargetCustomerId :: Lens.Lens' TransferringInputDeviceSummary (Lude.Maybe Lude.Text)
tidsTargetCustomerId = Lens.lens (targetCustomerId :: TransferringInputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {targetCustomerId = a} :: TransferringInputDeviceSummary)
{-# DEPRECATED tidsTargetCustomerId "Use generic-lens or generic-optics with 'targetCustomerId' instead." #-}

-- | The optional message that the sender has attached to the transfer.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidsMessage :: Lens.Lens' TransferringInputDeviceSummary (Lude.Maybe Lude.Text)
tidsMessage = Lens.lens (message :: TransferringInputDeviceSummary -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: TransferringInputDeviceSummary)
{-# DEPRECATED tidsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON TransferringInputDeviceSummary where
  parseJSON =
    Lude.withObject
      "TransferringInputDeviceSummary"
      ( \x ->
          TransferringInputDeviceSummary'
            Lude.<$> (x Lude..:? "transferType")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "targetCustomerId")
            Lude.<*> (x Lude..:? "message")
      )
