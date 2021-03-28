{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies a user's permissions. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingsecurity.html Security and Permissions> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.SetPermission
    (
    -- * Creating a request
      SetPermission (..)
    , mkSetPermission
    -- ** Request lenses
    , spStackId
    , spIamUserArn
    , spAllowSsh
    , spAllowSudo
    , spLevel

    -- * Destructuring the response
    , SetPermissionResponse (..)
    , mkSetPermissionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetPermission' smart constructor.
data SetPermission = SetPermission'
  { stackId :: Core.Text
    -- ^ The stack ID.
  , iamUserArn :: Core.Text
    -- ^ The user's IAM ARN. This can also be a federated user's ARN.
  , allowSsh :: Core.Maybe Core.Bool
    -- ^ The user is allowed to use SSH to communicate with the instance.
  , allowSudo :: Core.Maybe Core.Bool
    -- ^ The user is allowed to use __sudo__ to elevate privileges.
  , level :: Core.Maybe Core.Text
    -- ^ The user's permission level, which must be set to one of the following strings. You cannot set your own permissions level.
--
--
--     * @deny@ 
--
--
--     * @show@ 
--
--
--     * @deploy@ 
--
--
--     * @manage@ 
--
--
--     * @iam_only@ 
--
--
-- For more information about the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetPermission' value with any optional fields omitted.
mkSetPermission
    :: Core.Text -- ^ 'stackId'
    -> Core.Text -- ^ 'iamUserArn'
    -> SetPermission
mkSetPermission stackId iamUserArn
  = SetPermission'{stackId, iamUserArn, allowSsh = Core.Nothing,
                   allowSudo = Core.Nothing, level = Core.Nothing}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spStackId :: Lens.Lens' SetPermission Core.Text
spStackId = Lens.field @"stackId"
{-# INLINEABLE spStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The user's IAM ARN. This can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spIamUserArn :: Lens.Lens' SetPermission Core.Text
spIamUserArn = Lens.field @"iamUserArn"
{-# INLINEABLE spIamUserArn #-}
{-# DEPRECATED iamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead"  #-}

-- | The user is allowed to use SSH to communicate with the instance.
--
-- /Note:/ Consider using 'allowSsh' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAllowSsh :: Lens.Lens' SetPermission (Core.Maybe Core.Bool)
spAllowSsh = Lens.field @"allowSsh"
{-# INLINEABLE spAllowSsh #-}
{-# DEPRECATED allowSsh "Use generic-lens or generic-optics with 'allowSsh' instead"  #-}

-- | The user is allowed to use __sudo__ to elevate privileges.
--
-- /Note:/ Consider using 'allowSudo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAllowSudo :: Lens.Lens' SetPermission (Core.Maybe Core.Bool)
spAllowSudo = Lens.field @"allowSudo"
{-# INLINEABLE spAllowSudo #-}
{-# DEPRECATED allowSudo "Use generic-lens or generic-optics with 'allowSudo' instead"  #-}

-- | The user's permission level, which must be set to one of the following strings. You cannot set your own permissions level.
--
--
--     * @deny@ 
--
--
--     * @show@ 
--
--
--     * @deploy@ 
--
--
--     * @manage@ 
--
--
--     * @iam_only@ 
--
--
-- For more information about the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spLevel :: Lens.Lens' SetPermission (Core.Maybe Core.Text)
spLevel = Lens.field @"level"
{-# INLINEABLE spLevel #-}
{-# DEPRECATED level "Use generic-lens or generic-optics with 'level' instead"  #-}

instance Core.ToQuery SetPermission where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetPermission where
        toHeaders SetPermission{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.SetPermission")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetPermission where
        toJSON SetPermission{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackId" Core..= stackId),
                  Core.Just ("IamUserArn" Core..= iamUserArn),
                  ("AllowSsh" Core..=) Core.<$> allowSsh,
                  ("AllowSudo" Core..=) Core.<$> allowSudo,
                  ("Level" Core..=) Core.<$> level])

instance Core.AWSRequest SetPermission where
        type Rs SetPermission = SetPermissionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetPermissionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetPermissionResponse' smart constructor.
data SetPermissionResponse = SetPermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetPermissionResponse' value with any optional fields omitted.
mkSetPermissionResponse
    :: SetPermissionResponse
mkSetPermissionResponse = SetPermissionResponse'
