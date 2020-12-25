{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AdminAccount
  ( AdminAccount (..),

    -- * Smart constructor
    mkAdminAccount,

    -- * Lenses
    aaAdminAccountId,
    aaAdminStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types.AdminStatus as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The account within the organization specified as the GuardDuty delegated administrator.
--
-- /See:/ 'mkAdminAccount' smart constructor.
data AdminAccount = AdminAccount'
  { -- | The AWS account ID for the account.
    adminAccountId :: Core.Maybe Types.String,
    -- | Indicates whether the account is enabled as the delegated administrator.
    adminStatus :: Core.Maybe Types.AdminStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminAccount' value with any optional fields omitted.
mkAdminAccount ::
  AdminAccount
mkAdminAccount =
  AdminAccount'
    { adminAccountId = Core.Nothing,
      adminStatus = Core.Nothing
    }

-- | The AWS account ID for the account.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAdminAccountId :: Lens.Lens' AdminAccount (Core.Maybe Types.String)
aaAdminAccountId = Lens.field @"adminAccountId"
{-# DEPRECATED aaAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

-- | Indicates whether the account is enabled as the delegated administrator.
--
-- /Note:/ Consider using 'adminStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAdminStatus :: Lens.Lens' AdminAccount (Core.Maybe Types.AdminStatus)
aaAdminStatus = Lens.field @"adminStatus"
{-# DEPRECATED aaAdminStatus "Use generic-lens or generic-optics with 'adminStatus' instead." #-}

instance Core.FromJSON AdminAccount where
  parseJSON =
    Core.withObject "AdminAccount" Core.$
      \x ->
        AdminAccount'
          Core.<$> (x Core..:? "adminAccountId") Core.<*> (x Core..:? "adminStatus")
