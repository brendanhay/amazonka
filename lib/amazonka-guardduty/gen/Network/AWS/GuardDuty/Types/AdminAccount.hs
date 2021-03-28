{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.AdminAccount
  ( AdminAccount (..)
  -- * Smart constructor
  , mkAdminAccount
  -- * Lenses
  , aaAdminAccountId
  , aaAdminStatus
  ) where

import qualified Network.AWS.GuardDuty.Types.AdminStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The account within the organization specified as the GuardDuty delegated administrator.
--
-- /See:/ 'mkAdminAccount' smart constructor.
data AdminAccount = AdminAccount'
  { adminAccountId :: Core.Maybe Core.Text
    -- ^ The AWS account ID for the account.
  , adminStatus :: Core.Maybe Types.AdminStatus
    -- ^ Indicates whether the account is enabled as the delegated administrator.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminAccount' value with any optional fields omitted.
mkAdminAccount
    :: AdminAccount
mkAdminAccount
  = AdminAccount'{adminAccountId = Core.Nothing,
                  adminStatus = Core.Nothing}

-- | The AWS account ID for the account.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAdminAccountId :: Lens.Lens' AdminAccount (Core.Maybe Core.Text)
aaAdminAccountId = Lens.field @"adminAccountId"
{-# INLINEABLE aaAdminAccountId #-}
{-# DEPRECATED adminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead"  #-}

-- | Indicates whether the account is enabled as the delegated administrator.
--
-- /Note:/ Consider using 'adminStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAdminStatus :: Lens.Lens' AdminAccount (Core.Maybe Types.AdminStatus)
aaAdminStatus = Lens.field @"adminStatus"
{-# INLINEABLE aaAdminStatus #-}
{-# DEPRECATED adminStatus "Use generic-lens or generic-optics with 'adminStatus' instead"  #-}

instance Core.FromJSON AdminAccount where
        parseJSON
          = Core.withObject "AdminAccount" Core.$
              \ x ->
                AdminAccount' Core.<$>
                  (x Core..:? "adminAccountId") Core.<*> x Core..:? "adminStatus"
