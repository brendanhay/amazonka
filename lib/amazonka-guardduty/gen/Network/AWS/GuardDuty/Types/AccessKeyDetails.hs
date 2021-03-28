{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccessKeyDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.AccessKeyDetails
  ( AccessKeyDetails (..)
  -- * Smart constructor
  , mkAccessKeyDetails
  -- * Lenses
  , akdAccessKeyId
  , akdPrincipalId
  , akdUserName
  , akdUserType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the access keys.
--
-- /See:/ 'mkAccessKeyDetails' smart constructor.
data AccessKeyDetails = AccessKeyDetails'
  { accessKeyId :: Core.Maybe Core.Text
    -- ^ The access key ID of the user.
  , principalId :: Core.Maybe Core.Text
    -- ^ The principal ID of the user.
  , userName :: Core.Maybe Core.Text
    -- ^ The name of the user.
  , userType :: Core.Maybe Core.Text
    -- ^ The type of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessKeyDetails' value with any optional fields omitted.
mkAccessKeyDetails
    :: AccessKeyDetails
mkAccessKeyDetails
  = AccessKeyDetails'{accessKeyId = Core.Nothing,
                      principalId = Core.Nothing, userName = Core.Nothing,
                      userType = Core.Nothing}

-- | The access key ID of the user.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdAccessKeyId :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
akdAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE akdAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

-- | The principal ID of the user.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdPrincipalId :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
akdPrincipalId = Lens.field @"principalId"
{-# INLINEABLE akdPrincipalId #-}
{-# DEPRECATED principalId "Use generic-lens or generic-optics with 'principalId' instead"  #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdUserName :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
akdUserName = Lens.field @"userName"
{-# INLINEABLE akdUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The type of the user.
--
-- /Note:/ Consider using 'userType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdUserType :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
akdUserType = Lens.field @"userType"
{-# INLINEABLE akdUserType #-}
{-# DEPRECATED userType "Use generic-lens or generic-optics with 'userType' instead"  #-}

instance Core.FromJSON AccessKeyDetails where
        parseJSON
          = Core.withObject "AccessKeyDetails" Core.$
              \ x ->
                AccessKeyDetails' Core.<$>
                  (x Core..:? "accessKeyId") Core.<*> x Core..:? "principalId"
                    Core.<*> x Core..:? "userName"
                    Core.<*> x Core..:? "userType"
