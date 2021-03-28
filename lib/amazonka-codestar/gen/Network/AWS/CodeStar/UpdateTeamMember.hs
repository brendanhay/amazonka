{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UpdateTeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a team member's attributes in an AWS CodeStar project. For example, you can change a team member's role in the project, or change whether they have remote access to project resources.
module Network.AWS.CodeStar.UpdateTeamMember
    (
    -- * Creating a request
      UpdateTeamMember (..)
    , mkUpdateTeamMember
    -- ** Request lenses
    , utmProjectId
    , utmUserArn
    , utmProjectRole
    , utmRemoteAccessAllowed

    -- * Destructuring the response
    , UpdateTeamMemberResponse (..)
    , mkUpdateTeamMemberResponse
    -- ** Response lenses
    , utmrrsProjectRole
    , utmrrsRemoteAccessAllowed
    , utmrrsUserArn
    , utmrrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTeamMember' smart constructor.
data UpdateTeamMember = UpdateTeamMember'
  { projectId :: Types.ProjectId
    -- ^ The ID of the project.
  , userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user for whom you want to change team membership attributes.
  , projectRole :: Core.Maybe Types.ProjectRole
    -- ^ The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
  , remoteAccessAllowed :: Core.Maybe Core.Bool
    -- ^ Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile. Even if this is set to True, the user must associate a public key with their profile before the user can access resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTeamMember' value with any optional fields omitted.
mkUpdateTeamMember
    :: Types.ProjectId -- ^ 'projectId'
    -> Types.UserArn -- ^ 'userArn'
    -> UpdateTeamMember
mkUpdateTeamMember projectId userArn
  = UpdateTeamMember'{projectId, userArn, projectRole = Core.Nothing,
                      remoteAccessAllowed = Core.Nothing}

-- | The ID of the project.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmProjectId :: Lens.Lens' UpdateTeamMember Types.ProjectId
utmProjectId = Lens.field @"projectId"
{-# INLINEABLE utmProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user for whom you want to change team membership attributes.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmUserArn :: Lens.Lens' UpdateTeamMember Types.UserArn
utmUserArn = Lens.field @"userArn"
{-# INLINEABLE utmUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmProjectRole :: Lens.Lens' UpdateTeamMember (Core.Maybe Types.ProjectRole)
utmProjectRole = Lens.field @"projectRole"
{-# INLINEABLE utmProjectRole #-}
{-# DEPRECATED projectRole "Use generic-lens or generic-optics with 'projectRole' instead"  #-}

-- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile. Even if this is set to True, the user must associate a public key with their profile before the user can access resources.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmRemoteAccessAllowed :: Lens.Lens' UpdateTeamMember (Core.Maybe Core.Bool)
utmRemoteAccessAllowed = Lens.field @"remoteAccessAllowed"
{-# INLINEABLE utmRemoteAccessAllowed #-}
{-# DEPRECATED remoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead"  #-}

instance Core.ToQuery UpdateTeamMember where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTeamMember where
        toHeaders UpdateTeamMember{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.UpdateTeamMember")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTeamMember where
        toJSON UpdateTeamMember{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectId" Core..= projectId),
                  Core.Just ("userArn" Core..= userArn),
                  ("projectRole" Core..=) Core.<$> projectRole,
                  ("remoteAccessAllowed" Core..=) Core.<$> remoteAccessAllowed])

instance Core.AWSRequest UpdateTeamMember where
        type Rs UpdateTeamMember = UpdateTeamMemberResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTeamMemberResponse' Core.<$>
                   (x Core..:? "projectRole") Core.<*>
                     x Core..:? "remoteAccessAllowed"
                     Core.<*> x Core..:? "userArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTeamMemberResponse' smart constructor.
data UpdateTeamMemberResponse = UpdateTeamMemberResponse'
  { projectRole :: Core.Maybe Types.Role
    -- ^ The project role granted to the user.
  , remoteAccessAllowed :: Core.Maybe Core.Bool
    -- ^ Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile.
  , userArn :: Core.Maybe Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user whose team membership attributes were updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTeamMemberResponse' value with any optional fields omitted.
mkUpdateTeamMemberResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTeamMemberResponse
mkUpdateTeamMemberResponse responseStatus
  = UpdateTeamMemberResponse'{projectRole = Core.Nothing,
                              remoteAccessAllowed = Core.Nothing, userArn = Core.Nothing,
                              responseStatus}

-- | The project role granted to the user.
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrrsProjectRole :: Lens.Lens' UpdateTeamMemberResponse (Core.Maybe Types.Role)
utmrrsProjectRole = Lens.field @"projectRole"
{-# INLINEABLE utmrrsProjectRole #-}
{-# DEPRECATED projectRole "Use generic-lens or generic-optics with 'projectRole' instead"  #-}

-- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrrsRemoteAccessAllowed :: Lens.Lens' UpdateTeamMemberResponse (Core.Maybe Core.Bool)
utmrrsRemoteAccessAllowed = Lens.field @"remoteAccessAllowed"
{-# INLINEABLE utmrrsRemoteAccessAllowed #-}
{-# DEPRECATED remoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user whose team membership attributes were updated.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrrsUserArn :: Lens.Lens' UpdateTeamMemberResponse (Core.Maybe Types.UserArn)
utmrrsUserArn = Lens.field @"userArn"
{-# INLINEABLE utmrrsUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrrsResponseStatus :: Lens.Lens' UpdateTeamMemberResponse Core.Int
utmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
