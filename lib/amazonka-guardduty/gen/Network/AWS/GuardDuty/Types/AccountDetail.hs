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

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.Email as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the account.
--
-- /See:/ 'mkAccountDetail' smart constructor.
data AccountDetail = AccountDetail'
  { -- | The member account ID.
    accountId :: Types.AccountId,
    -- | The email address of the member account.
    email :: Types.Email
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountDetail' value with any optional fields omitted.
mkAccountDetail ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'email'
  Types.Email ->
  AccountDetail
mkAccountDetail accountId email = AccountDetail' {accountId, email}

-- | The member account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAccountId :: Lens.Lens' AccountDetail Types.AccountId
adAccountId = Lens.field @"accountId"
{-# DEPRECATED adAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The email address of the member account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEmail :: Lens.Lens' AccountDetail Types.Email
adEmail = Lens.field @"email"
{-# DEPRECATED adEmail "Use generic-lens or generic-optics with 'email' instead." #-}

instance Core.FromJSON AccountDetail where
  toJSON AccountDetail {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("accountId" Core..= accountId),
            Core.Just ("email" Core..= email)
          ]
      )
