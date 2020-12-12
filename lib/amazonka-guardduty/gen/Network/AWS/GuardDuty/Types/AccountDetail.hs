{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccountDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccountDetail
  ( AccountDetail (..),

    -- * Smart constructor
    mkAccountDetail,

    -- * Lenses
    adAccountId,
    adEmail,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the account.
--
-- /See:/ 'mkAccountDetail' smart constructor.
data AccountDetail = AccountDetail'
  { accountId :: Lude.Text,
    email :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountDetail' with the minimum fields required to make a request.
--
-- * 'accountId' - The member account ID.
-- * 'email' - The email address of the member account.
mkAccountDetail ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'email'
  Lude.Text ->
  AccountDetail
mkAccountDetail pAccountId_ pEmail_ =
  AccountDetail' {accountId = pAccountId_, email = pEmail_}

-- | The member account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAccountId :: Lens.Lens' AccountDetail Lude.Text
adAccountId = Lens.lens (accountId :: AccountDetail -> Lude.Text) (\s a -> s {accountId = a} :: AccountDetail)
{-# DEPRECATED adAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The email address of the member account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEmail :: Lens.Lens' AccountDetail Lude.Text
adEmail = Lens.lens (email :: AccountDetail -> Lude.Text) (\s a -> s {email = a} :: AccountDetail)
{-# DEPRECATED adEmail "Use generic-lens or generic-optics with 'email' instead." #-}

instance Lude.ToJSON AccountDetail where
  toJSON AccountDetail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("accountId" Lude..= accountId),
            Lude.Just ("email" Lude..= email)
          ]
      )
