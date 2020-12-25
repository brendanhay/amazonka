{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ciInitiatorName,
    ciSecretToAuthenticateInitiator,
    ciSecretToAuthenticateTarget,
    ciTargetARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.InitiatorName as Types
import qualified Network.AWS.StorageGateway.Types.SecretToAuthenticateInitiator as Types
import qualified Network.AWS.StorageGateway.Types.SecretToAuthenticateTarget as Types
import qualified Network.AWS.StorageGateway.Types.TargetARN as Types

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information that supports authentication between your gateway and iSCSI initiators.
--
-- /See:/ 'mkChapInfo' smart constructor.
data ChapInfo = ChapInfo'
  { -- | The iSCSI initiator that connects to the target.
    initiatorName :: Core.Maybe Types.InitiatorName,
    -- | The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
    secretToAuthenticateInitiator :: Core.Maybe Types.SecretToAuthenticateInitiator,
    -- | The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g., Windows client).
    secretToAuthenticateTarget :: Core.Maybe Types.SecretToAuthenticateTarget,
    -- | The Amazon Resource Name (ARN) of the volume.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
    targetARN :: Core.Maybe Types.TargetARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChapInfo' value with any optional fields omitted.
mkChapInfo ::
  ChapInfo
mkChapInfo =
  ChapInfo'
    { initiatorName = Core.Nothing,
      secretToAuthenticateInitiator = Core.Nothing,
      secretToAuthenticateTarget = Core.Nothing,
      targetARN = Core.Nothing
    }

-- | The iSCSI initiator that connects to the target.
--
-- /Note:/ Consider using 'initiatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInitiatorName :: Lens.Lens' ChapInfo (Core.Maybe Types.InitiatorName)
ciInitiatorName = Lens.field @"initiatorName"
{-# DEPRECATED ciInitiatorName "Use generic-lens or generic-optics with 'initiatorName' instead." #-}

-- | The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
-- /Note:/ Consider using 'secretToAuthenticateInitiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSecretToAuthenticateInitiator :: Lens.Lens' ChapInfo (Core.Maybe Types.SecretToAuthenticateInitiator)
ciSecretToAuthenticateInitiator = Lens.field @"secretToAuthenticateInitiator"
{-# DEPRECATED ciSecretToAuthenticateInitiator "Use generic-lens or generic-optics with 'secretToAuthenticateInitiator' instead." #-}

-- | The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g., Windows client).
--
-- /Note:/ Consider using 'secretToAuthenticateTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSecretToAuthenticateTarget :: Lens.Lens' ChapInfo (Core.Maybe Types.SecretToAuthenticateTarget)
ciSecretToAuthenticateTarget = Lens.field @"secretToAuthenticateTarget"
{-# DEPRECATED ciSecretToAuthenticateTarget "Use generic-lens or generic-optics with 'secretToAuthenticateTarget' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTargetARN :: Lens.Lens' ChapInfo (Core.Maybe Types.TargetARN)
ciTargetARN = Lens.field @"targetARN"
{-# DEPRECATED ciTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

instance Core.FromJSON ChapInfo where
  parseJSON =
    Core.withObject "ChapInfo" Core.$
      \x ->
        ChapInfo'
          Core.<$> (x Core..:? "InitiatorName")
          Core.<*> (x Core..:? "SecretToAuthenticateInitiator")
          Core.<*> (x Core..:? "SecretToAuthenticateTarget")
          Core.<*> (x Core..:? "TargetARN")
