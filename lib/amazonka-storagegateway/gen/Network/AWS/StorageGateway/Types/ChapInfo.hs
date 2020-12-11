-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.ChapInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.ChapInfo
  ( ChapInfo (..),

    -- * Smart constructor
    mkChapInfo,

    -- * Lenses
    ciTargetARN,
    ciSecretToAuthenticateInitiator,
    ciInitiatorName,
    ciSecretToAuthenticateTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information that supports authentication between your gateway and iSCSI initiators.
--
-- /See:/ 'mkChapInfo' smart constructor.
data ChapInfo = ChapInfo'
  { targetARN :: Lude.Maybe Lude.Text,
    secretToAuthenticateInitiator ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    initiatorName :: Lude.Maybe Lude.Text,
    secretToAuthenticateTarget ::
      Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChapInfo' with the minimum fields required to make a request.
--
-- * 'initiatorName' - The iSCSI initiator that connects to the target.
-- * 'secretToAuthenticateInitiator' - The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
-- * 'secretToAuthenticateTarget' - The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g., Windows client).
-- * 'targetARN' - The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
mkChapInfo ::
  ChapInfo
mkChapInfo =
  ChapInfo'
    { targetARN = Lude.Nothing,
      secretToAuthenticateInitiator = Lude.Nothing,
      initiatorName = Lude.Nothing,
      secretToAuthenticateTarget = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTargetARN :: Lens.Lens' ChapInfo (Lude.Maybe Lude.Text)
ciTargetARN = Lens.lens (targetARN :: ChapInfo -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: ChapInfo)
{-# DEPRECATED ciTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
-- /Note:/ Consider using 'secretToAuthenticateInitiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSecretToAuthenticateInitiator :: Lens.Lens' ChapInfo (Lude.Maybe (Lude.Sensitive Lude.Text))
ciSecretToAuthenticateInitiator = Lens.lens (secretToAuthenticateInitiator :: ChapInfo -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretToAuthenticateInitiator = a} :: ChapInfo)
{-# DEPRECATED ciSecretToAuthenticateInitiator "Use generic-lens or generic-optics with 'secretToAuthenticateInitiator' instead." #-}

-- | The iSCSI initiator that connects to the target.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInitiatorName :: Lens.Lens' ChapInfo (Lude.Maybe Lude.Text)
ciInitiatorName = Lens.lens (initiatorName :: ChapInfo -> Lude.Maybe Lude.Text) (\s a -> s {initiatorName = a} :: ChapInfo)
{-# DEPRECATED ciInitiatorName "Use generic-lens or generic-optics with 'initiatorName' instead." #-}

-- | The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g., Windows client).
--
-- /Note:/ Consider using 'secretToAuthenticateTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSecretToAuthenticateTarget :: Lens.Lens' ChapInfo (Lude.Maybe (Lude.Sensitive Lude.Text))
ciSecretToAuthenticateTarget = Lens.lens (secretToAuthenticateTarget :: ChapInfo -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretToAuthenticateTarget = a} :: ChapInfo)
{-# DEPRECATED ciSecretToAuthenticateTarget "Use generic-lens or generic-optics with 'secretToAuthenticateTarget' instead." #-}

instance Lude.FromJSON ChapInfo where
  parseJSON =
    Lude.withObject
      "ChapInfo"
      ( \x ->
          ChapInfo'
            Lude.<$> (x Lude..:? "TargetARN")
            Lude.<*> (x Lude..:? "SecretToAuthenticateInitiator")
            Lude.<*> (x Lude..:? "InitiatorName")
            Lude.<*> (x Lude..:? "SecretToAuthenticateTarget")
      )
