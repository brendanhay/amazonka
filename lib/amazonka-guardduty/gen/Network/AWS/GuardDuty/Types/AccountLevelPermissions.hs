-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccountLevelPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccountLevelPermissions
  ( AccountLevelPermissions (..),

    -- * Smart constructor
    mkAccountLevelPermissions,

    -- * Lenses
    alpBlockPublicAccess,
  )
where

import Network.AWS.GuardDuty.Types.BlockPublicAccess
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the account level permissions on the S3 bucket.
--
-- /See:/ 'mkAccountLevelPermissions' smart constructor.
newtype AccountLevelPermissions = AccountLevelPermissions'
  { blockPublicAccess ::
      Lude.Maybe BlockPublicAccess
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountLevelPermissions' with the minimum fields required to make a request.
--
-- * 'blockPublicAccess' - Describes the S3 Block Public Access settings of the bucket's parent account.
mkAccountLevelPermissions ::
  AccountLevelPermissions
mkAccountLevelPermissions =
  AccountLevelPermissions' {blockPublicAccess = Lude.Nothing}

-- | Describes the S3 Block Public Access settings of the bucket's parent account.
--
-- /Note:/ Consider using 'blockPublicAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alpBlockPublicAccess :: Lens.Lens' AccountLevelPermissions (Lude.Maybe BlockPublicAccess)
alpBlockPublicAccess = Lens.lens (blockPublicAccess :: AccountLevelPermissions -> Lude.Maybe BlockPublicAccess) (\s a -> s {blockPublicAccess = a} :: AccountLevelPermissions)
{-# DEPRECATED alpBlockPublicAccess "Use generic-lens or generic-optics with 'blockPublicAccess' instead." #-}

instance Lude.FromJSON AccountLevelPermissions where
  parseJSON =
    Lude.withObject
      "AccountLevelPermissions"
      ( \x ->
          AccountLevelPermissions' Lude.<$> (x Lude..:? "blockPublicAccess")
      )
