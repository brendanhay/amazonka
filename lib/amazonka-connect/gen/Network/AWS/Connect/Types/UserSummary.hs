{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.UserSummary
  ( UserSummary (..)
  -- * Smart constructor
  , mkUserSummary
  -- * Lenses
  , usArn
  , usId
  , usUsername
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.AgentUsername as Types
import qualified Network.AWS.Connect.Types.UserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a user.
--
-- /See:/ 'mkUserSummary' smart constructor.
data UserSummary = UserSummary'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the user account.
  , id :: Core.Maybe Types.UserId
    -- ^ The identifier of the user account.
  , username :: Core.Maybe Types.AgentUsername
    -- ^ The Amazon Connect user name of the user account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserSummary' value with any optional fields omitted.
mkUserSummary
    :: UserSummary
mkUserSummary
  = UserSummary'{arn = Core.Nothing, id = Core.Nothing,
                 username = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usArn :: Lens.Lens' UserSummary (Core.Maybe Types.ARN)
usArn = Lens.field @"arn"
{-# INLINEABLE usArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usId :: Lens.Lens' UserSummary (Core.Maybe Types.UserId)
usId = Lens.field @"id"
{-# INLINEABLE usId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Amazon Connect user name of the user account.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUsername :: Lens.Lens' UserSummary (Core.Maybe Types.AgentUsername)
usUsername = Lens.field @"username"
{-# INLINEABLE usUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON UserSummary where
        parseJSON
          = Core.withObject "UserSummary" Core.$
              \ x ->
                UserSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Username"
