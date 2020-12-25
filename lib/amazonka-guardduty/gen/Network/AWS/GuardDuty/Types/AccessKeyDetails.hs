{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccessKeyDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccessKeyDetails
  ( AccessKeyDetails (..),

    -- * Smart constructor
    mkAccessKeyDetails,

    -- * Lenses
    akdAccessKeyId,
    akdPrincipalId,
    akdUserName,
    akdUserType,
  )
where

import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the access keys.
--
-- /See:/ 'mkAccessKeyDetails' smart constructor.
data AccessKeyDetails = AccessKeyDetails'
  { -- | The access key ID of the user.
    accessKeyId :: Core.Maybe Types.String,
    -- | The principal ID of the user.
    principalId :: Core.Maybe Types.String,
    -- | The name of the user.
    userName :: Core.Maybe Types.String,
    -- | The type of the user.
    userType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessKeyDetails' value with any optional fields omitted.
mkAccessKeyDetails ::
  AccessKeyDetails
mkAccessKeyDetails =
  AccessKeyDetails'
    { accessKeyId = Core.Nothing,
      principalId = Core.Nothing,
      userName = Core.Nothing,
      userType = Core.Nothing
    }

-- | The access key ID of the user.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdAccessKeyId :: Lens.Lens' AccessKeyDetails (Core.Maybe Types.String)
akdAccessKeyId = Lens.field @"accessKeyId"
{-# DEPRECATED akdAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The principal ID of the user.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdPrincipalId :: Lens.Lens' AccessKeyDetails (Core.Maybe Types.String)
akdPrincipalId = Lens.field @"principalId"
{-# DEPRECATED akdPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdUserName :: Lens.Lens' AccessKeyDetails (Core.Maybe Types.String)
akdUserName = Lens.field @"userName"
{-# DEPRECATED akdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The type of the user.
--
-- /Note:/ Consider using 'userType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akdUserType :: Lens.Lens' AccessKeyDetails (Core.Maybe Types.String)
akdUserType = Lens.field @"userType"
{-# DEPRECATED akdUserType "Use generic-lens or generic-optics with 'userType' instead." #-}

instance Core.FromJSON AccessKeyDetails where
  parseJSON =
    Core.withObject "AccessKeyDetails" Core.$
      \x ->
        AccessKeyDetails'
          Core.<$> (x Core..:? "accessKeyId")
          Core.<*> (x Core..:? "principalId")
          Core.<*> (x Core..:? "userName")
          Core.<*> (x Core..:? "userType")
