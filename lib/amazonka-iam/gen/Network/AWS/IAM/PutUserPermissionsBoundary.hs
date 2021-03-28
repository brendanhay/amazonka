{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutUserPermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM user's permissions boundary. You can use an AWS managed policy or a customer managed policy to set the boundary for a user. Use the boundary to control the maximum permissions that the user can have. Setting a permissions boundary is an advanced feature that can affect the permissions for the user.
--
-- /Important:/ Policies that are used as permissions boundaries do not provide permissions. You must also attach a permissions policy to the user. To learn how the effective permissions for a user are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON Policy Evaluation Logic> in the IAM User Guide. 
module Network.AWS.IAM.PutUserPermissionsBoundary
    (
    -- * Creating a request
      PutUserPermissionsBoundary (..)
    , mkPutUserPermissionsBoundary
    -- ** Request lenses
    , pupbUserName
    , pupbPermissionsBoundary

    -- * Destructuring the response
    , PutUserPermissionsBoundaryResponse (..)
    , mkPutUserPermissionsBoundaryResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutUserPermissionsBoundary' smart constructor.
data PutUserPermissionsBoundary = PutUserPermissionsBoundary'
  { userName :: Types.UserNameType
    -- ^ The name (friendly name, not ARN) of the IAM user for which you want to set the permissions boundary.
  , permissionsBoundary :: Types.ArnType
    -- ^ The ARN of the policy that is used to set the permissions boundary for the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutUserPermissionsBoundary' value with any optional fields omitted.
mkPutUserPermissionsBoundary
    :: Types.UserNameType -- ^ 'userName'
    -> Types.ArnType -- ^ 'permissionsBoundary'
    -> PutUserPermissionsBoundary
mkPutUserPermissionsBoundary userName permissionsBoundary
  = PutUserPermissionsBoundary'{userName, permissionsBoundary}

-- | The name (friendly name, not ARN) of the IAM user for which you want to set the permissions boundary.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupbUserName :: Lens.Lens' PutUserPermissionsBoundary Types.UserNameType
pupbUserName = Lens.field @"userName"
{-# INLINEABLE pupbUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The ARN of the policy that is used to set the permissions boundary for the user.
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupbPermissionsBoundary :: Lens.Lens' PutUserPermissionsBoundary Types.ArnType
pupbPermissionsBoundary = Lens.field @"permissionsBoundary"
{-# INLINEABLE pupbPermissionsBoundary #-}
{-# DEPRECATED permissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead"  #-}

instance Core.ToQuery PutUserPermissionsBoundary where
        toQuery PutUserPermissionsBoundary{..}
          = Core.toQueryPair "Action"
              ("PutUserPermissionsBoundary" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "PermissionsBoundary" permissionsBoundary

instance Core.ToHeaders PutUserPermissionsBoundary where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutUserPermissionsBoundary where
        type Rs PutUserPermissionsBoundary =
             PutUserPermissionsBoundaryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutUserPermissionsBoundaryResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutUserPermissionsBoundaryResponse' smart constructor.
data PutUserPermissionsBoundaryResponse = PutUserPermissionsBoundaryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutUserPermissionsBoundaryResponse' value with any optional fields omitted.
mkPutUserPermissionsBoundaryResponse
    :: PutUserPermissionsBoundaryResponse
mkPutUserPermissionsBoundaryResponse
  = PutUserPermissionsBoundaryResponse'
