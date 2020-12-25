{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.TemporaryCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.TemporaryCredential
  ( TemporaryCredential (..),

    -- * Smart constructor
    mkTemporaryCredential,

    -- * Lenses
    tcInstanceId,
    tcPassword,
    tcUsername,
    tcValidForInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.
--
-- /See:/ 'mkTemporaryCredential' smart constructor.
data TemporaryCredential = TemporaryCredential'
  { -- | The instance's AWS OpsWorks Stacks ID.
    instanceId :: Core.Maybe Types.String,
    -- | The password.
    password :: Core.Maybe Types.String,
    -- | The user name.
    username :: Core.Maybe Types.String,
    -- | The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
    validForInMinutes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemporaryCredential' value with any optional fields omitted.
mkTemporaryCredential ::
  TemporaryCredential
mkTemporaryCredential =
  TemporaryCredential'
    { instanceId = Core.Nothing,
      password = Core.Nothing,
      username = Core.Nothing,
      validForInMinutes = Core.Nothing
    }

-- | The instance's AWS OpsWorks Stacks ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInstanceId :: Lens.Lens' TemporaryCredential (Core.Maybe Types.String)
tcInstanceId = Lens.field @"instanceId"
{-# DEPRECATED tcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcPassword :: Lens.Lens' TemporaryCredential (Core.Maybe Types.String)
tcPassword = Lens.field @"password"
{-# DEPRECATED tcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcUsername :: Lens.Lens' TemporaryCredential (Core.Maybe Types.String)
tcUsername = Lens.field @"username"
{-# DEPRECATED tcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
--
-- /Note:/ Consider using 'validForInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcValidForInMinutes :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Int)
tcValidForInMinutes = Lens.field @"validForInMinutes"
{-# DEPRECATED tcValidForInMinutes "Use generic-lens or generic-optics with 'validForInMinutes' instead." #-}

instance Core.FromJSON TemporaryCredential where
  parseJSON =
    Core.withObject "TemporaryCredential" Core.$
      \x ->
        TemporaryCredential'
          Core.<$> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "Password")
          Core.<*> (x Core..:? "Username")
          Core.<*> (x Core..:? "ValidForInMinutes")
