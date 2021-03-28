{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing work team with new member definitions or description.
module Network.AWS.SageMaker.UpdateWorkteam
    (
    -- * Creating a request
      UpdateWorkteam (..)
    , mkUpdateWorkteam
    -- ** Request lenses
    , uwWorkteamName
    , uwDescription
    , uwMemberDefinitions
    , uwNotificationConfiguration

    -- * Destructuring the response
    , UpdateWorkteamResponse (..)
    , mkUpdateWorkteamResponse
    -- ** Response lenses
    , uwrrsWorkteam
    , uwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateWorkteam' smart constructor.
data UpdateWorkteam = UpdateWorkteam'
  { workteamName :: Types.WorkteamName
    -- ^ The name of the work team to update.
  , description :: Core.Maybe Types.String200
    -- ^ An updated description for the work team.
  , memberDefinitions :: Core.Maybe (Core.NonEmpty Types.MemberDefinition)
    -- ^ A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team. 
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . You should not provide input for both of these parameters in a single request.
-- For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> .
-- For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ . Be aware that user groups that are already in the work team must also be listed in @Groups@ when you make this request to remain on the work team. If you do not include these user groups, they will no longer be associated with the work team you update. 
  , notificationConfiguration :: Core.Maybe Types.NotificationConfiguration
    -- ^ Configures SNS topic notifications for available or expiring work items
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkteam' value with any optional fields omitted.
mkUpdateWorkteam
    :: Types.WorkteamName -- ^ 'workteamName'
    -> UpdateWorkteam
mkUpdateWorkteam workteamName
  = UpdateWorkteam'{workteamName, description = Core.Nothing,
                    memberDefinitions = Core.Nothing,
                    notificationConfiguration = Core.Nothing}

-- | The name of the work team to update.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwWorkteamName :: Lens.Lens' UpdateWorkteam Types.WorkteamName
uwWorkteamName = Lens.field @"workteamName"
{-# INLINEABLE uwWorkteamName #-}
{-# DEPRECATED workteamName "Use generic-lens or generic-optics with 'workteamName' instead"  #-}

-- | An updated description for the work team.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwDescription :: Lens.Lens' UpdateWorkteam (Core.Maybe Types.String200)
uwDescription = Lens.field @"description"
{-# INLINEABLE uwDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team. 
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . You should not provide input for both of these parameters in a single request.
-- For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> .
-- For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ . Be aware that user groups that are already in the work team must also be listed in @Groups@ when you make this request to remain on the work team. If you do not include these user groups, they will no longer be associated with the work team you update. 
--
-- /Note:/ Consider using 'memberDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwMemberDefinitions :: Lens.Lens' UpdateWorkteam (Core.Maybe (Core.NonEmpty Types.MemberDefinition))
uwMemberDefinitions = Lens.field @"memberDefinitions"
{-# INLINEABLE uwMemberDefinitions #-}
{-# DEPRECATED memberDefinitions "Use generic-lens or generic-optics with 'memberDefinitions' instead"  #-}

-- | Configures SNS topic notifications for available or expiring work items
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwNotificationConfiguration :: Lens.Lens' UpdateWorkteam (Core.Maybe Types.NotificationConfiguration)
uwNotificationConfiguration = Lens.field @"notificationConfiguration"
{-# INLINEABLE uwNotificationConfiguration #-}
{-# DEPRECATED notificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead"  #-}

instance Core.ToQuery UpdateWorkteam where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateWorkteam where
        toHeaders UpdateWorkteam{..}
          = Core.pure ("X-Amz-Target", "SageMaker.UpdateWorkteam") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateWorkteam where
        toJSON UpdateWorkteam{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkteamName" Core..= workteamName),
                  ("Description" Core..=) Core.<$> description,
                  ("MemberDefinitions" Core..=) Core.<$> memberDefinitions,
                  ("NotificationConfiguration" Core..=) Core.<$>
                    notificationConfiguration])

instance Core.AWSRequest UpdateWorkteam where
        type Rs UpdateWorkteam = UpdateWorkteamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateWorkteamResponse' Core.<$>
                   (x Core..: "Workteam") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateWorkteamResponse' smart constructor.
data UpdateWorkteamResponse = UpdateWorkteamResponse'
  { workteam :: Types.Workteam
    -- ^ A @Workteam@ object that describes the updated work team.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateWorkteamResponse' value with any optional fields omitted.
mkUpdateWorkteamResponse
    :: Types.Workteam -- ^ 'workteam'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateWorkteamResponse
mkUpdateWorkteamResponse workteam responseStatus
  = UpdateWorkteamResponse'{workteam, responseStatus}

-- | A @Workteam@ object that describes the updated work team.
--
-- /Note:/ Consider using 'workteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrrsWorkteam :: Lens.Lens' UpdateWorkteamResponse Types.Workteam
uwrrsWorkteam = Lens.field @"workteam"
{-# INLINEABLE uwrrsWorkteam #-}
{-# DEPRECATED workteam "Use generic-lens or generic-optics with 'workteam' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrrsResponseStatus :: Lens.Lens' UpdateWorkteamResponse Core.Int
uwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
