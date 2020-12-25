{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UnprocessedAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UnprocessedAccount
  ( UnprocessedAccount (..),

    -- * Smart constructor
    mkUnprocessedAccount,

    -- * Lenses
    uaAccountId,
    uaResult,
  )
where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the accounts that weren't processed.
--
-- /See:/ 'mkUnprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { -- | The AWS account ID.
    accountId :: Types.AccountId,
    -- | A reason why the account hasn't been processed.
    result :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedAccount' value with any optional fields omitted.
mkUnprocessedAccount ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'result'
  Types.String ->
  UnprocessedAccount
mkUnprocessedAccount accountId result =
  UnprocessedAccount' {accountId, result}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAccountId :: Lens.Lens' UnprocessedAccount Types.AccountId
uaAccountId = Lens.field @"accountId"
{-# DEPRECATED uaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A reason why the account hasn't been processed.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaResult :: Lens.Lens' UnprocessedAccount Types.String
uaResult = Lens.field @"result"
{-# DEPRECATED uaResult "Use generic-lens or generic-optics with 'result' instead." #-}

instance Core.FromJSON UnprocessedAccount where
  parseJSON =
    Core.withObject "UnprocessedAccount" Core.$
      \x ->
        UnprocessedAccount'
          Core.<$> (x Core..: "accountId") Core.<*> (x Core..: "result")
