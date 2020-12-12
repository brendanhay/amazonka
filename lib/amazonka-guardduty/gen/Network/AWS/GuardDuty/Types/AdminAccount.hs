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

import Network.AWS.GuardDuty.Types.AdminStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The account within the organization specified as the GuardDuty delegated administrator.
--
-- /See:/ 'mkAdminAccount' smart constructor.
data AdminAccount = AdminAccount'
  { adminAccountId ::
      Lude.Maybe Lude.Text,
    adminStatus :: Lude.Maybe AdminStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminAccount' with the minimum fields required to make a request.
--
-- * 'adminAccountId' - The AWS account ID for the account.
-- * 'adminStatus' - Indicates whether the account is enabled as the delegated administrator.
mkAdminAccount ::
  AdminAccount
mkAdminAccount =
  AdminAccount'
    { adminAccountId = Lude.Nothing,
      adminStatus = Lude.Nothing
    }

-- | The AWS account ID for the account.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAdminAccountId :: Lens.Lens' AdminAccount (Lude.Maybe Lude.Text)
aaAdminAccountId = Lens.lens (adminAccountId :: AdminAccount -> Lude.Maybe Lude.Text) (\s a -> s {adminAccountId = a} :: AdminAccount)
{-# DEPRECATED aaAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

-- | Indicates whether the account is enabled as the delegated administrator.
--
-- /Note:/ Consider using 'adminStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAdminStatus :: Lens.Lens' AdminAccount (Lude.Maybe AdminStatus)
aaAdminStatus = Lens.lens (adminStatus :: AdminAccount -> Lude.Maybe AdminStatus) (\s a -> s {adminStatus = a} :: AdminAccount)
{-# DEPRECATED aaAdminStatus "Use generic-lens or generic-optics with 'adminStatus' instead." #-}

instance Lude.FromJSON AdminAccount where
  parseJSON =
    Lude.withObject
      "AdminAccount"
      ( \x ->
          AdminAccount'
            Lude.<$> (x Lude..:? "adminAccountId") Lude.<*> (x Lude..:? "adminStatus")
      )
