{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.AssociateTeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an IAM user to the team for an AWS CodeStar project.
module Network.AWS.CodeStar.AssociateTeamMember
    (
    -- * Creating a request
      AssociateTeamMember (..)
    , mkAssociateTeamMember
    -- ** Request lenses
    , atmProjectId
    , atmUserArn
    , atmProjectRole
    , atmClientRequestToken
    , atmRemoteAccessAllowed

    -- * Destructuring the response
    , AssociateTeamMemberResponse (..)
    , mkAssociateTeamMemberResponse
    -- ** Response lenses
    , atmrrsClientRequestToken
    , atmrrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateTeamMember' smart constructor.
data AssociateTeamMember = AssociateTeamMember'
  { projectId :: Types.ProjectId
    -- ^ The ID of the project to which you will add the IAM user.
  , userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) for the IAM user you want to add to the AWS CodeStar project.
  , projectRole :: Types.ProjectRole
    -- ^ The AWS CodeStar project role that will apply to this user. This role determines what actions a user can take in an AWS CodeStar project.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A user- or system-generated token that identifies the entity that requested the team member association to the project. This token can be used to repeat the request.
  , remoteAccessAllowed :: Core.Maybe Core.Bool
    -- ^ Whether the team member is allowed to use an SSH public/private key pair to remotely access project resources, for example Amazon EC2 instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTeamMember' value with any optional fields omitted.
mkAssociateTeamMember
    :: Types.ProjectId -- ^ 'projectId'
    -> Types.UserArn -- ^ 'userArn'
    -> Types.ProjectRole -- ^ 'projectRole'
    -> AssociateTeamMember
mkAssociateTeamMember projectId userArn projectRole
  = AssociateTeamMember'{projectId, userArn, projectRole,
                         clientRequestToken = Core.Nothing,
                         remoteAccessAllowed = Core.Nothing}

-- | The ID of the project to which you will add the IAM user.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmProjectId :: Lens.Lens' AssociateTeamMember Types.ProjectId
atmProjectId = Lens.field @"projectId"
{-# INLINEABLE atmProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

-- | The Amazon Resource Name (ARN) for the IAM user you want to add to the AWS CodeStar project.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmUserArn :: Lens.Lens' AssociateTeamMember Types.UserArn
atmUserArn = Lens.field @"userArn"
{-# INLINEABLE atmUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The AWS CodeStar project role that will apply to this user. This role determines what actions a user can take in an AWS CodeStar project.
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmProjectRole :: Lens.Lens' AssociateTeamMember Types.ProjectRole
atmProjectRole = Lens.field @"projectRole"
{-# INLINEABLE atmProjectRole #-}
{-# DEPRECATED projectRole "Use generic-lens or generic-optics with 'projectRole' instead"  #-}

-- | A user- or system-generated token that identifies the entity that requested the team member association to the project. This token can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmClientRequestToken :: Lens.Lens' AssociateTeamMember (Core.Maybe Types.ClientRequestToken)
atmClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE atmClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Whether the team member is allowed to use an SSH public/private key pair to remotely access project resources, for example Amazon EC2 instances.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmRemoteAccessAllowed :: Lens.Lens' AssociateTeamMember (Core.Maybe Core.Bool)
atmRemoteAccessAllowed = Lens.field @"remoteAccessAllowed"
{-# INLINEABLE atmRemoteAccessAllowed #-}
{-# DEPRECATED remoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead"  #-}

instance Core.ToQuery AssociateTeamMember where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateTeamMember where
        toHeaders AssociateTeamMember{..}
          = Core.pure
              ("X-Amz-Target", "CodeStar_20170419.AssociateTeamMember")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateTeamMember where
        toJSON AssociateTeamMember{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectId" Core..= projectId),
                  Core.Just ("userArn" Core..= userArn),
                  Core.Just ("projectRole" Core..= projectRole),
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("remoteAccessAllowed" Core..=) Core.<$> remoteAccessAllowed])

instance Core.AWSRequest AssociateTeamMember where
        type Rs AssociateTeamMember = AssociateTeamMemberResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssociateTeamMemberResponse' Core.<$>
                   (x Core..:? "clientRequestToken") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateTeamMemberResponse' smart constructor.
data AssociateTeamMemberResponse = AssociateTeamMemberResponse'
  { clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ The user- or system-generated token from the initial request that can be used to repeat the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTeamMemberResponse' value with any optional fields omitted.
mkAssociateTeamMemberResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateTeamMemberResponse
mkAssociateTeamMemberResponse responseStatus
  = AssociateTeamMemberResponse'{clientRequestToken = Core.Nothing,
                                 responseStatus}

-- | The user- or system-generated token from the initial request that can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmrrsClientRequestToken :: Lens.Lens' AssociateTeamMemberResponse (Core.Maybe Types.ClientRequestToken)
atmrrsClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE atmrrsClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmrrsResponseStatus :: Lens.Lens' AssociateTeamMemberResponse Core.Int
atmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
