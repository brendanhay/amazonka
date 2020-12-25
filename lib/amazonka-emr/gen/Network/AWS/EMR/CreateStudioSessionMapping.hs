{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.CreateStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Maps a user or group to the Amazon EMR Studio specified by @StudioId@ , and applies a session policy to refine Studio permissions for that user or group.
module Network.AWS.EMR.CreateStudioSessionMapping
  ( -- * Creating a request
    CreateStudioSessionMapping (..),
    mkCreateStudioSessionMapping,

    -- ** Request lenses
    cssmStudioId,
    cssmIdentityType,
    cssmSessionPolicyArn,
    cssmIdentityId,
    cssmIdentityName,

    -- * Destructuring the response
    CreateStudioSessionMappingResponse (..),
    mkCreateStudioSessionMappingResponse,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStudioSessionMapping' smart constructor.
data CreateStudioSessionMapping = CreateStudioSessionMapping'
  { -- | The ID of the Amazon EMR Studio to which the user or group will be mapped.
    studioId :: Types.StudioId,
    -- | Specifies whether the identity to map to the Studio is a user or a group.
    identityType :: Types.IdentityType,
    -- | The Amazon Resource Name (ARN) for the session policy that will be applied to the user or group. Session policies refine Studio user permissions without the need to use multiple IAM user roles.
    sessionPolicyArn :: Types.SessionPolicyArn,
    -- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
    identityId :: Core.Maybe Types.IdentityId,
    -- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
    identityName :: Core.Maybe Types.IdentityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStudioSessionMapping' value with any optional fields omitted.
mkCreateStudioSessionMapping ::
  -- | 'studioId'
  Types.StudioId ->
  -- | 'identityType'
  Types.IdentityType ->
  -- | 'sessionPolicyArn'
  Types.SessionPolicyArn ->
  CreateStudioSessionMapping
mkCreateStudioSessionMapping studioId identityType sessionPolicyArn =
  CreateStudioSessionMapping'
    { studioId,
      identityType,
      sessionPolicyArn,
      identityId = Core.Nothing,
      identityName = Core.Nothing
    }

-- | The ID of the Amazon EMR Studio to which the user or group will be mapped.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmStudioId :: Lens.Lens' CreateStudioSessionMapping Types.StudioId
cssmStudioId = Lens.field @"studioId"
{-# DEPRECATED cssmStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | Specifies whether the identity to map to the Studio is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmIdentityType :: Lens.Lens' CreateStudioSessionMapping Types.IdentityType
cssmIdentityType = Lens.field @"identityType"
{-# DEPRECATED cssmIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The Amazon Resource Name (ARN) for the session policy that will be applied to the user or group. Session policies refine Studio user permissions without the need to use multiple IAM user roles.
--
-- /Note:/ Consider using 'sessionPolicyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmSessionPolicyArn :: Lens.Lens' CreateStudioSessionMapping Types.SessionPolicyArn
cssmSessionPolicyArn = Lens.field @"sessionPolicyArn"
{-# DEPRECATED cssmSessionPolicyArn "Use generic-lens or generic-optics with 'sessionPolicyArn' instead." #-}

-- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmIdentityId :: Lens.Lens' CreateStudioSessionMapping (Core.Maybe Types.IdentityId)
cssmIdentityId = Lens.field @"identityId"
{-# DEPRECATED cssmIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssmIdentityName :: Lens.Lens' CreateStudioSessionMapping (Core.Maybe Types.IdentityName)
cssmIdentityName = Lens.field @"identityName"
{-# DEPRECATED cssmIdentityName "Use generic-lens or generic-optics with 'identityName' instead." #-}

instance Core.FromJSON CreateStudioSessionMapping where
  toJSON CreateStudioSessionMapping {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StudioId" Core..= studioId),
            Core.Just ("IdentityType" Core..= identityType),
            Core.Just ("SessionPolicyArn" Core..= sessionPolicyArn),
            ("IdentityId" Core..=) Core.<$> identityId,
            ("IdentityName" Core..=) Core.<$> identityName
          ]
      )

instance Core.AWSRequest CreateStudioSessionMapping where
  type
    Rs CreateStudioSessionMapping =
      CreateStudioSessionMappingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.CreateStudioSessionMapping")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CreateStudioSessionMappingResponse'

-- | /See:/ 'mkCreateStudioSessionMappingResponse' smart constructor.
data CreateStudioSessionMappingResponse = CreateStudioSessionMappingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStudioSessionMappingResponse' value with any optional fields omitted.
mkCreateStudioSessionMappingResponse ::
  CreateStudioSessionMappingResponse
mkCreateStudioSessionMappingResponse =
  CreateStudioSessionMappingResponse'
