{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccountLevelPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.AccountLevelPermissions
  ( AccountLevelPermissions (..)
  -- * Smart constructor
  , mkAccountLevelPermissions
  -- * Lenses
  , alpBlockPublicAccess
  ) where

import qualified Network.AWS.GuardDuty.Types.BlockPublicAccess as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the account level permissions on the S3 bucket.
--
-- /See:/ 'mkAccountLevelPermissions' smart constructor.
newtype AccountLevelPermissions = AccountLevelPermissions'
  { blockPublicAccess :: Core.Maybe Types.BlockPublicAccess
    -- ^ Describes the S3 Block Public Access settings of the bucket's parent account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AccountLevelPermissions' value with any optional fields omitted.
mkAccountLevelPermissions
    :: AccountLevelPermissions
mkAccountLevelPermissions
  = AccountLevelPermissions'{blockPublicAccess = Core.Nothing}

-- | Describes the S3 Block Public Access settings of the bucket's parent account.
--
-- /Note:/ Consider using 'blockPublicAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alpBlockPublicAccess :: Lens.Lens' AccountLevelPermissions (Core.Maybe Types.BlockPublicAccess)
alpBlockPublicAccess = Lens.field @"blockPublicAccess"
{-# INLINEABLE alpBlockPublicAccess #-}
{-# DEPRECATED blockPublicAccess "Use generic-lens or generic-optics with 'blockPublicAccess' instead"  #-}

instance Core.FromJSON AccountLevelPermissions where
        parseJSON
          = Core.withObject "AccountLevelPermissions" Core.$
              \ x ->
                AccountLevelPermissions' Core.<$> (x Core..:? "blockPublicAccess")
