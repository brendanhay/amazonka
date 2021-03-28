{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description or maximum session duration setting of a role.
module Network.AWS.IAM.UpdateRole
    (
    -- * Creating a request
      UpdateRole (..)
    , mkUpdateRole
    -- ** Request lenses
    , urRoleName
    , urDescription
    , urMaxSessionDuration

    -- * Destructuring the response
    , UpdateRoleResponse (..)
    , mkUpdateRoleResponse
    -- ** Response lenses
    , urrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRole' smart constructor.
data UpdateRole = UpdateRole'
  { roleName :: Types.RoleName
    -- ^ The name of the role that you want to modify.
  , description :: Core.Maybe Types.Description
    -- ^ The new description that you want to apply to the specified role.
  , maxSessionDuration :: Core.Maybe Core.Natural
    -- ^ The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRole' value with any optional fields omitted.
mkUpdateRole
    :: Types.RoleName -- ^ 'roleName'
    -> UpdateRole
mkUpdateRole roleName
  = UpdateRole'{roleName, description = Core.Nothing,
                maxSessionDuration = Core.Nothing}

-- | The name of the role that you want to modify.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoleName :: Lens.Lens' UpdateRole Types.RoleName
urRoleName = Lens.field @"roleName"
{-# INLINEABLE urRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The new description that you want to apply to the specified role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRole (Core.Maybe Types.Description)
urDescription = Lens.field @"description"
{-# INLINEABLE urDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'maxSessionDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urMaxSessionDuration :: Lens.Lens' UpdateRole (Core.Maybe Core.Natural)
urMaxSessionDuration = Lens.field @"maxSessionDuration"
{-# INLINEABLE urMaxSessionDuration #-}
{-# DEPRECATED maxSessionDuration "Use generic-lens or generic-optics with 'maxSessionDuration' instead"  #-}

instance Core.ToQuery UpdateRole where
        toQuery UpdateRole{..}
          = Core.toQueryPair "Action" ("UpdateRole" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxSessionDuration")
                maxSessionDuration

instance Core.ToHeaders UpdateRole where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateRole where
        type Rs UpdateRole = UpdateRoleResponse
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
          = Response.receiveXMLWrapper "UpdateRoleResult"
              (\ s h x ->
                 UpdateRoleResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRoleResponse' smart constructor.
newtype UpdateRoleResponse = UpdateRoleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoleResponse' value with any optional fields omitted.
mkUpdateRoleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRoleResponse
mkUpdateRoleResponse responseStatus
  = UpdateRoleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateRoleResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
