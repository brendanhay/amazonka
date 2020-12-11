-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AccountSharingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AccountSharingInfo
  ( AccountSharingInfo (..),

    -- * Smart constructor
    mkAccountSharingInfo,

    -- * Lenses
    asiSharedDocumentVersion,
    asiAccountId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information includes the AWS account ID where the current document is shared and the version shared with that account.
--
-- /See:/ 'mkAccountSharingInfo' smart constructor.
data AccountSharingInfo = AccountSharingInfo'
  { sharedDocumentVersion ::
      Lude.Maybe Lude.Text,
    accountId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountSharingInfo' with the minimum fields required to make a request.
--
-- * 'accountId' - The AWS account ID where the current document is shared.
-- * 'sharedDocumentVersion' - The version of the current document shared with the account.
mkAccountSharingInfo ::
  AccountSharingInfo
mkAccountSharingInfo =
  AccountSharingInfo'
    { sharedDocumentVersion = Lude.Nothing,
      accountId = Lude.Nothing
    }

-- | The version of the current document shared with the account.
--
-- /Note:/ Consider using 'sharedDocumentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiSharedDocumentVersion :: Lens.Lens' AccountSharingInfo (Lude.Maybe Lude.Text)
asiSharedDocumentVersion = Lens.lens (sharedDocumentVersion :: AccountSharingInfo -> Lude.Maybe Lude.Text) (\s a -> s {sharedDocumentVersion = a} :: AccountSharingInfo)
{-# DEPRECATED asiSharedDocumentVersion "Use generic-lens or generic-optics with 'sharedDocumentVersion' instead." #-}

-- | The AWS account ID where the current document is shared.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiAccountId :: Lens.Lens' AccountSharingInfo (Lude.Maybe Lude.Text)
asiAccountId = Lens.lens (accountId :: AccountSharingInfo -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: AccountSharingInfo)
{-# DEPRECATED asiAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.FromJSON AccountSharingInfo where
  parseJSON =
    Lude.withObject
      "AccountSharingInfo"
      ( \x ->
          AccountSharingInfo'
            Lude.<$> (x Lude..:? "SharedDocumentVersion")
            Lude.<*> (x Lude..:? "AccountId")
      )
