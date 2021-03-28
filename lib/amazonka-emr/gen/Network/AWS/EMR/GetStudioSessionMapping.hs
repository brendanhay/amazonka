{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.GetStudioSessionMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches mapping details for the specified Amazon EMR Studio and identity (user or group).
module Network.AWS.EMR.GetStudioSessionMapping
    (
    -- * Creating a request
      GetStudioSessionMapping (..)
    , mkGetStudioSessionMapping
    -- ** Request lenses
    , gssmStudioId
    , gssmIdentityType
    , gssmIdentityId
    , gssmIdentityName

    -- * Destructuring the response
    , GetStudioSessionMappingResponse (..)
    , mkGetStudioSessionMappingResponse
    -- ** Response lenses
    , gssmrrsSessionMapping
    , gssmrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetStudioSessionMapping' smart constructor.
data GetStudioSessionMapping = GetStudioSessionMapping'
  { studioId :: Types.StudioId
    -- ^ The ID of the Amazon EMR Studio.
  , identityType :: Types.IdentityType
    -- ^ Specifies whether the identity to fetch is a user or a group.
  , identityId :: Core.Maybe Types.IdentityId
    -- ^ The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
  , identityName :: Core.Maybe Types.IdentityName
    -- ^ The name of the user or group to fetch. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStudioSessionMapping' value with any optional fields omitted.
mkGetStudioSessionMapping
    :: Types.StudioId -- ^ 'studioId'
    -> Types.IdentityType -- ^ 'identityType'
    -> GetStudioSessionMapping
mkGetStudioSessionMapping studioId identityType
  = GetStudioSessionMapping'{studioId, identityType,
                             identityId = Core.Nothing, identityName = Core.Nothing}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmStudioId :: Lens.Lens' GetStudioSessionMapping Types.StudioId
gssmStudioId = Lens.field @"studioId"
{-# INLINEABLE gssmStudioId #-}
{-# DEPRECATED studioId "Use generic-lens or generic-optics with 'studioId' instead"  #-}

-- | Specifies whether the identity to fetch is a user or a group.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmIdentityType :: Lens.Lens' GetStudioSessionMapping Types.IdentityType
gssmIdentityType = Lens.field @"identityType"
{-# INLINEABLE gssmIdentityType #-}
{-# DEPRECATED identityType "Use generic-lens or generic-optics with 'identityType' instead"  #-}

-- | The globally unique identifier (GUID) of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmIdentityId :: Lens.Lens' GetStudioSessionMapping (Core.Maybe Types.IdentityId)
gssmIdentityId = Lens.field @"identityId"
{-# INLINEABLE gssmIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The name of the user or group to fetch. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ . Either @IdentityName@ or @IdentityId@ must be specified.
--
-- /Note:/ Consider using 'identityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmIdentityName :: Lens.Lens' GetStudioSessionMapping (Core.Maybe Types.IdentityName)
gssmIdentityName = Lens.field @"identityName"
{-# INLINEABLE gssmIdentityName #-}
{-# DEPRECATED identityName "Use generic-lens or generic-optics with 'identityName' instead"  #-}

instance Core.ToQuery GetStudioSessionMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetStudioSessionMapping where
        toHeaders GetStudioSessionMapping{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.GetStudioSessionMapping")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetStudioSessionMapping where
        toJSON GetStudioSessionMapping{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StudioId" Core..= studioId),
                  Core.Just ("IdentityType" Core..= identityType),
                  ("IdentityId" Core..=) Core.<$> identityId,
                  ("IdentityName" Core..=) Core.<$> identityName])

instance Core.AWSRequest GetStudioSessionMapping where
        type Rs GetStudioSessionMapping = GetStudioSessionMappingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetStudioSessionMappingResponse' Core.<$>
                   (x Core..:? "SessionMapping") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetStudioSessionMappingResponse' smart constructor.
data GetStudioSessionMappingResponse = GetStudioSessionMappingResponse'
  { sessionMapping :: Core.Maybe Types.SessionMappingDetail
    -- ^ The session mapping details for the specified Amazon EMR Studio and identity, including session policy ARN and creation time.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetStudioSessionMappingResponse' value with any optional fields omitted.
mkGetStudioSessionMappingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetStudioSessionMappingResponse
mkGetStudioSessionMappingResponse responseStatus
  = GetStudioSessionMappingResponse'{sessionMapping = Core.Nothing,
                                     responseStatus}

-- | The session mapping details for the specified Amazon EMR Studio and identity, including session policy ARN and creation time.
--
-- /Note:/ Consider using 'sessionMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmrrsSessionMapping :: Lens.Lens' GetStudioSessionMappingResponse (Core.Maybe Types.SessionMappingDetail)
gssmrrsSessionMapping = Lens.field @"sessionMapping"
{-# INLINEABLE gssmrrsSessionMapping #-}
{-# DEPRECATED sessionMapping "Use generic-lens or generic-optics with 'sessionMapping' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssmrrsResponseStatus :: Lens.Lens' GetStudioSessionMappingResponse Core.Int
gssmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gssmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
