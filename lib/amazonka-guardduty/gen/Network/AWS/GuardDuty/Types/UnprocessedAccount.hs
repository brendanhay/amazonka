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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the accounts that weren't processed.
--
-- /See:/ 'mkUnprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { accountId ::
      Lude.Text,
    result :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnprocessedAccount' with the minimum fields required to make a request.
--
-- * 'accountId' - The AWS account ID.
-- * 'result' - A reason why the account hasn't been processed.
mkUnprocessedAccount ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'result'
  Lude.Text ->
  UnprocessedAccount
mkUnprocessedAccount pAccountId_ pResult_ =
  UnprocessedAccount' {accountId = pAccountId_, result = pResult_}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAccountId :: Lens.Lens' UnprocessedAccount Lude.Text
uaAccountId = Lens.lens (accountId :: UnprocessedAccount -> Lude.Text) (\s a -> s {accountId = a} :: UnprocessedAccount)
{-# DEPRECATED uaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A reason why the account hasn't been processed.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaResult :: Lens.Lens' UnprocessedAccount Lude.Text
uaResult = Lens.lens (result :: UnprocessedAccount -> Lude.Text) (\s a -> s {result = a} :: UnprocessedAccount)
{-# DEPRECATED uaResult "Use generic-lens or generic-optics with 'result' instead." #-}

instance Lude.FromJSON UnprocessedAccount where
  parseJSON =
    Lude.withObject
      "UnprocessedAccount"
      ( \x ->
          UnprocessedAccount'
            Lude.<$> (x Lude..: "accountId") Lude.<*> (x Lude..: "result")
      )
