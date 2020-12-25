{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceCredentials
  ( InstanceCredentials (..),

    -- * Smart constructor
    mkInstanceCredentials,

    -- * Lenses
    icSecret,
    icUserName,
  )
where

import qualified Network.AWS.GameLift.Types.Secret as Types
import qualified Network.AWS.GameLift.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Set of credentials required to remotely access a fleet instance. Access credentials are requested by calling 'GetInstanceAccess' and returned in an 'InstanceAccess' object.
--
-- /See:/ 'mkInstanceCredentials' smart constructor.
data InstanceCredentials = InstanceCredentials'
  { -- | Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
    secret :: Core.Maybe Types.Secret,
    -- | User login string.
    userName :: Core.Maybe Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceCredentials' value with any optional fields omitted.
mkInstanceCredentials ::
  InstanceCredentials
mkInstanceCredentials =
  InstanceCredentials'
    { secret = Core.Nothing,
      userName = Core.Nothing
    }

-- | Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
--
-- /Note:/ Consider using 'secret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSecret :: Lens.Lens' InstanceCredentials (Core.Maybe Types.Secret)
icSecret = Lens.field @"secret"
{-# DEPRECATED icSecret "Use generic-lens or generic-optics with 'secret' instead." #-}

-- | User login string.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icUserName :: Lens.Lens' InstanceCredentials (Core.Maybe Types.UserName)
icUserName = Lens.field @"userName"
{-# DEPRECATED icUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.FromJSON InstanceCredentials where
  parseJSON =
    Core.withObject "InstanceCredentials" Core.$
      \x ->
        InstanceCredentials'
          Core.<$> (x Core..:? "Secret") Core.<*> (x Core..:? "UserName")
