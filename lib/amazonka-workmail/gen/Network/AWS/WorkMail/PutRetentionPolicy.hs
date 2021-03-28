{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.PutRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a retention policy to the specified organization.
module Network.AWS.WorkMail.PutRetentionPolicy
    (
    -- * Creating a request
      PutRetentionPolicy (..)
    , mkPutRetentionPolicy
    -- ** Request lenses
    , prpOrganizationId
    , prpName
    , prpFolderConfigurations
    , prpDescription
    , prpId

    -- * Destructuring the response
    , PutRetentionPolicyResponse (..)
    , mkPutRetentionPolicyResponse
    -- ** Response lenses
    , prprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { organizationId :: Types.OrganizationId
    -- ^ The organization ID.
  , name :: Types.Name
    -- ^ The retention policy name.
  , folderConfigurations :: [Types.FolderConfiguration]
    -- ^ The retention policy folder configurations.
  , description :: Core.Maybe Types.PolicyDescription
    -- ^ The retention policy description.
  , id :: Core.Maybe Types.Id
    -- ^ The retention policy ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRetentionPolicy' value with any optional fields omitted.
mkPutRetentionPolicy
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.Name -- ^ 'name'
    -> PutRetentionPolicy
mkPutRetentionPolicy organizationId name
  = PutRetentionPolicy'{organizationId, name,
                        folderConfigurations = Core.mempty, description = Core.Nothing,
                        id = Core.Nothing}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpOrganizationId :: Lens.Lens' PutRetentionPolicy Types.OrganizationId
prpOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE prpOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The retention policy name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpName :: Lens.Lens' PutRetentionPolicy Types.Name
prpName = Lens.field @"name"
{-# INLINEABLE prpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The retention policy folder configurations.
--
-- /Note:/ Consider using 'folderConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpFolderConfigurations :: Lens.Lens' PutRetentionPolicy [Types.FolderConfiguration]
prpFolderConfigurations = Lens.field @"folderConfigurations"
{-# INLINEABLE prpFolderConfigurations #-}
{-# DEPRECATED folderConfigurations "Use generic-lens or generic-optics with 'folderConfigurations' instead"  #-}

-- | The retention policy description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpDescription :: Lens.Lens' PutRetentionPolicy (Core.Maybe Types.PolicyDescription)
prpDescription = Lens.field @"description"
{-# INLINEABLE prpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpId :: Lens.Lens' PutRetentionPolicy (Core.Maybe Types.Id)
prpId = Lens.field @"id"
{-# INLINEABLE prpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery PutRetentionPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutRetentionPolicy where
        toHeaders PutRetentionPolicy{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.PutRetentionPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutRetentionPolicy where
        toJSON PutRetentionPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("Name" Core..= name),
                  Core.Just ("FolderConfigurations" Core..= folderConfigurations),
                  ("Description" Core..=) Core.<$> description,
                  ("Id" Core..=) Core.<$> id])

instance Core.AWSRequest PutRetentionPolicy where
        type Rs PutRetentionPolicy = PutRetentionPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutRetentionPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutRetentionPolicyResponse' smart constructor.
newtype PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutRetentionPolicyResponse' value with any optional fields omitted.
mkPutRetentionPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutRetentionPolicyResponse
mkPutRetentionPolicyResponse responseStatus
  = PutRetentionPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResponseStatus :: Lens.Lens' PutRetentionPolicyResponse Core.Int
prprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
