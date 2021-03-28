{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.UpdateStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the session policy attached to the user or group for the specified Amazon EMR Studio.
module Network.AWS.EMR.UpdateStudioSessionMapping
    (
    -- * Creating a request
      UpdateStudioSessionMapping (..)
    , mkUpdateStudioSessionMapping
    -- ** Request lenses
    , ussmStudioId
    , ussmIdentityType
    , ussmSessionPolicyArn
    , ussmIdentityId
    , ussmIdentityName

    -- * Destructuring the response
    , UpdateStudioSessionMappingResponse (..)
    , mkUpdateStudioSessionMappingResponse
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateStudioSessionMapping' smart constructor.
data UpdateStudioSessionMapping = UpdateStudioSessionMapping'
  { studioId :: Types.XmlStringMaxLen256
    -- ^ The ID of the EMR Studio.
  , identityType :: Types.IdentityType
    -- ^ Specifies whether the identity to update is a user or a group.
  , sessionPolicyArn :: Types.XmlStringMaxLen256
    -- ^ The Amazon Resource Name (ARN) of the session policy to associate with the specified user or group.
  , identityId :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
  , identityName :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The name of the user or group to update. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStudioSessionMapping' value with any optional fields omitted.
mkUpdateStudioSessionMapping
    :: Types.XmlStringMaxLen256 -- ^ 'studioId'
    -> Types.IdentityType -- ^ 'identityType'
    -> Types.XmlStringMaxLen256 -- ^ 'sessionPolicyArn'
    -> UpdateStudioSessionMapping
mkUpdateStudioSessionMapping studioId identityType sessionPolicyArn
  = UpdateStudioSessionMapping'{studioId, identityType,
                                sessionPolicyArn, identityId = Core.Nothing,
                                identityName = Core.Nothing}

-- | The ID of the EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmStudioId :: Lens.Lens' UpdateStudioSessionMapping Types.XmlStringMaxLen256
ussmStudioId = Lens.field @"studioId"
{-# INLINEABLE ussmStudioId #-}
{-# DEPRECATED studioId "Use generic-lens or generic-optics with 'studioId' instead"  #-}

-- | Specifies whether the identity to update is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmIdentityType :: Lens.Lens' UpdateStudioSessionMapping Types.IdentityType
ussmIdentityType = Lens.field @"identityType"
{-# INLINEABLE ussmIdentityType #-}
{-# DEPRECATED identityType "Use generic-lens or generic-optics with 'identityType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the session policy to associate with the specified user or group.
--
-- /Note:/ Consider using 'sessionPolicyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmSessionPolicyArn :: Lens.Lens' UpdateStudioSessionMapping Types.XmlStringMaxLen256
ussmSessionPolicyArn = Lens.field @"sessionPolicyArn"
{-# INLINEABLE ussmSessionPolicyArn #-}
{-# DEPRECATED sessionPolicyArn "Use generic-lens or generic-optics with 'sessionPolicyArn' instead"  #-}

-- | The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmIdentityId :: Lens.Lens' UpdateStudioSessionMapping (Core.Maybe Types.XmlStringMaxLen256)
ussmIdentityId = Lens.field @"identityId"
{-# INLINEABLE ussmIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The name of the user or group to update. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussmIdentityName :: Lens.Lens' UpdateStudioSessionMapping (Core.Maybe Types.XmlStringMaxLen256)
ussmIdentityName = Lens.field @"identityName"
{-# INLINEABLE ussmIdentityName #-}
{-# DEPRECATED identityName "Use generic-lens or generic-optics with 'identityName' instead"  #-}

instance Core.ToQuery UpdateStudioSessionMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateStudioSessionMapping where
        toHeaders UpdateStudioSessionMapping{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.UpdateStudioSessionMapping")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateStudioSessionMapping where
        toJSON UpdateStudioSessionMapping{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StudioId" Core..= studioId),
                  Core.Just ("IdentityType" Core..= identityType),
                  Core.Just ("SessionPolicyArn" Core..= sessionPolicyArn),
                  ("IdentityId" Core..=) Core.<$> identityId,
                  ("IdentityName" Core..=) Core.<$> identityName])

instance Core.AWSRequest UpdateStudioSessionMapping where
        type Rs UpdateStudioSessionMapping =
             UpdateStudioSessionMappingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull UpdateStudioSessionMappingResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateStudioSessionMappingResponse' smart constructor.
data UpdateStudioSessionMappingResponse = UpdateStudioSessionMappingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStudioSessionMappingResponse' value with any optional fields omitted.
mkUpdateStudioSessionMappingResponse
    :: UpdateStudioSessionMappingResponse
mkUpdateStudioSessionMappingResponse
  = UpdateStudioSessionMappingResponse'
