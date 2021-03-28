{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.UserSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.UserSummary
  ( UserSummary (..)
  -- * Smart constructor
  , mkUserSummary
  -- * Lenses
  , usPendingChange
  , usUsername
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.ChangeType as Types
import qualified Network.AWS.Prelude as Core

-- | Returns a list of all broker users.
--
-- /See:/ 'mkUserSummary' smart constructor.
data UserSummary = UserSummary'
  { pendingChange :: Core.Maybe Types.ChangeType
    -- ^ The type of change pending for the broker user.
  , username :: Core.Maybe Core.Text
    -- ^ Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserSummary' value with any optional fields omitted.
mkUserSummary
    :: UserSummary
mkUserSummary
  = UserSummary'{pendingChange = Core.Nothing,
                 username = Core.Nothing}

-- | The type of change pending for the broker user.
--
-- /Note:/ Consider using 'pendingChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPendingChange :: Lens.Lens' UserSummary (Core.Maybe Types.ChangeType)
usPendingChange = Lens.field @"pendingChange"
{-# INLINEABLE usPendingChange #-}
{-# DEPRECATED pendingChange "Use generic-lens or generic-optics with 'pendingChange' instead"  #-}

-- | Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUsername :: Lens.Lens' UserSummary (Core.Maybe Core.Text)
usUsername = Lens.field @"username"
{-# INLINEABLE usUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON UserSummary where
        parseJSON
          = Core.withObject "UserSummary" Core.$
              \ x ->
                UserSummary' Core.<$>
                  (x Core..:? "pendingChange") Core.<*> x Core..:? "username"
