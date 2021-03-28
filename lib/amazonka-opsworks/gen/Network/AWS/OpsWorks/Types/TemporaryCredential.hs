{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.TemporaryCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.TemporaryCredential
  ( TemporaryCredential (..)
  -- * Smart constructor
  , mkTemporaryCredential
  -- * Lenses
  , tcInstanceId
  , tcPassword
  , tcUsername
  , tcValidForInMinutes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.
--
-- /See:/ 'mkTemporaryCredential' smart constructor.
data TemporaryCredential = TemporaryCredential'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The instance's AWS OpsWorks Stacks ID.
  , password :: Core.Maybe Core.Text
    -- ^ The password.
  , username :: Core.Maybe Core.Text
    -- ^ The user name.
  , validForInMinutes :: Core.Maybe Core.Int
    -- ^ The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemporaryCredential' value with any optional fields omitted.
mkTemporaryCredential
    :: TemporaryCredential
mkTemporaryCredential
  = TemporaryCredential'{instanceId = Core.Nothing,
                         password = Core.Nothing, username = Core.Nothing,
                         validForInMinutes = Core.Nothing}

-- | The instance's AWS OpsWorks Stacks ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInstanceId :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Text)
tcInstanceId = Lens.field @"instanceId"
{-# INLINEABLE tcInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcPassword :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Text)
tcPassword = Lens.field @"password"
{-# INLINEABLE tcPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcUsername :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Text)
tcUsername = Lens.field @"username"
{-# INLINEABLE tcUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
--
-- /Note:/ Consider using 'validForInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcValidForInMinutes :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Int)
tcValidForInMinutes = Lens.field @"validForInMinutes"
{-# INLINEABLE tcValidForInMinutes #-}
{-# DEPRECATED validForInMinutes "Use generic-lens or generic-optics with 'validForInMinutes' instead"  #-}

instance Core.FromJSON TemporaryCredential where
        parseJSON
          = Core.withObject "TemporaryCredential" Core.$
              \ x ->
                TemporaryCredential' Core.<$>
                  (x Core..:? "InstanceId") Core.<*> x Core..:? "Password" Core.<*>
                    x Core..:? "Username"
                    Core.<*> x Core..:? "ValidForInMinutes"
