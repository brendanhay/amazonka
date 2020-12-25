{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AccountWithRestoreAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AccountWithRestoreAccess
  ( AccountWithRestoreAccess (..),

    -- * Smart constructor
    mkAccountWithRestoreAccess,

    -- * Lenses
    awraAccountAlias,
    awraAccountId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes an AWS customer account authorized to restore a snapshot.
--
-- /See:/ 'mkAccountWithRestoreAccess' smart constructor.
data AccountWithRestoreAccess = AccountWithRestoreAccess'
  { -- | The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
    accountAlias :: Core.Maybe Types.String,
    -- | The identifier of an AWS customer account authorized to restore a snapshot.
    accountId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountWithRestoreAccess' value with any optional fields omitted.
mkAccountWithRestoreAccess ::
  AccountWithRestoreAccess
mkAccountWithRestoreAccess =
  AccountWithRestoreAccess'
    { accountAlias = Core.Nothing,
      accountId = Core.Nothing
    }

-- | The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
--
-- /Note:/ Consider using 'accountAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awraAccountAlias :: Lens.Lens' AccountWithRestoreAccess (Core.Maybe Types.String)
awraAccountAlias = Lens.field @"accountAlias"
{-# DEPRECATED awraAccountAlias "Use generic-lens or generic-optics with 'accountAlias' instead." #-}

-- | The identifier of an AWS customer account authorized to restore a snapshot.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awraAccountId :: Lens.Lens' AccountWithRestoreAccess (Core.Maybe Types.String)
awraAccountId = Lens.field @"accountId"
{-# DEPRECATED awraAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Core.FromXML AccountWithRestoreAccess where
  parseXML x =
    AccountWithRestoreAccess'
      Core.<$> (x Core..@? "AccountAlias") Core.<*> (x Core..@? "AccountId")
