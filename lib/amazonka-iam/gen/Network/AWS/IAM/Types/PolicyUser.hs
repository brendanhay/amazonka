{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.PolicyUser
  ( PolicyUser (..)
  -- * Smart constructor
  , mkPolicyUser
  -- * Lenses
  , puUserId
  , puUserName
  ) where

import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.UserNameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a user that a managed policy is attached to.
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation. 
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ . 
--
-- /See:/ 'mkPolicyUser' smart constructor.
data PolicyUser = PolicyUser'
  { userId :: Core.Maybe Types.IdType
    -- ^ The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
  , userName :: Core.Maybe Types.UserNameType
    -- ^ The name (friendly name, not ARN) identifying the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyUser' value with any optional fields omitted.
mkPolicyUser
    :: PolicyUser
mkPolicyUser
  = PolicyUser'{userId = Core.Nothing, userName = Core.Nothing}

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puUserId :: Lens.Lens' PolicyUser (Core.Maybe Types.IdType)
puUserId = Lens.field @"userId"
{-# INLINEABLE puUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The name (friendly name, not ARN) identifying the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puUserName :: Lens.Lens' PolicyUser (Core.Maybe Types.UserNameType)
puUserName = Lens.field @"userName"
{-# INLINEABLE puUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.FromXML PolicyUser where
        parseXML x
          = PolicyUser' Core.<$>
              (x Core..@? "UserId") Core.<*> x Core..@? "UserName"
