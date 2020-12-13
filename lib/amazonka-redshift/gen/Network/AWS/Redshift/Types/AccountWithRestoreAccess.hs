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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes an AWS customer account authorized to restore a snapshot.
--
-- /See:/ 'mkAccountWithRestoreAccess' smart constructor.
data AccountWithRestoreAccess = AccountWithRestoreAccess'
  { -- | The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
    accountAlias :: Lude.Maybe Lude.Text,
    -- | The identifier of an AWS customer account authorized to restore a snapshot.
    accountId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountWithRestoreAccess' with the minimum fields required to make a request.
--
-- * 'accountAlias' - The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
-- * 'accountId' - The identifier of an AWS customer account authorized to restore a snapshot.
mkAccountWithRestoreAccess ::
  AccountWithRestoreAccess
mkAccountWithRestoreAccess =
  AccountWithRestoreAccess'
    { accountAlias = Lude.Nothing,
      accountId = Lude.Nothing
    }

-- | The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
--
-- /Note:/ Consider using 'accountAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awraAccountAlias :: Lens.Lens' AccountWithRestoreAccess (Lude.Maybe Lude.Text)
awraAccountAlias = Lens.lens (accountAlias :: AccountWithRestoreAccess -> Lude.Maybe Lude.Text) (\s a -> s {accountAlias = a} :: AccountWithRestoreAccess)
{-# DEPRECATED awraAccountAlias "Use generic-lens or generic-optics with 'accountAlias' instead." #-}

-- | The identifier of an AWS customer account authorized to restore a snapshot.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awraAccountId :: Lens.Lens' AccountWithRestoreAccess (Lude.Maybe Lude.Text)
awraAccountId = Lens.lens (accountId :: AccountWithRestoreAccess -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: AccountWithRestoreAccess)
{-# DEPRECATED awraAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.FromXML AccountWithRestoreAccess where
  parseXML x =
    AccountWithRestoreAccess'
      Lude.<$> (x Lude..@? "AccountAlias") Lude.<*> (x Lude..@? "AccountId")
