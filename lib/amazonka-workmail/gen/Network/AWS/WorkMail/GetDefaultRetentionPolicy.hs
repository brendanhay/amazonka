{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.GetDefaultRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the default retention policy details for the specified organization.
module Network.AWS.WorkMail.GetDefaultRetentionPolicy
    (
    -- * Creating a request
      GetDefaultRetentionPolicy (..)
    , mkGetDefaultRetentionPolicy
    -- ** Request lenses
    , gdrpOrganizationId

    -- * Destructuring the response
    , GetDefaultRetentionPolicyResponse (..)
    , mkGetDefaultRetentionPolicyResponse
    -- ** Response lenses
    , gdrprrsDescription
    , gdrprrsFolderConfigurations
    , gdrprrsId
    , gdrprrsName
    , gdrprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkGetDefaultRetentionPolicy' smart constructor.
newtype GetDefaultRetentionPolicy = GetDefaultRetentionPolicy'
  { organizationId :: Types.OrganizationId
    -- ^ The organization ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultRetentionPolicy' value with any optional fields omitted.
mkGetDefaultRetentionPolicy
    :: Types.OrganizationId -- ^ 'organizationId'
    -> GetDefaultRetentionPolicy
mkGetDefaultRetentionPolicy organizationId
  = GetDefaultRetentionPolicy'{organizationId}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrpOrganizationId :: Lens.Lens' GetDefaultRetentionPolicy Types.OrganizationId
gdrpOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE gdrpOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

instance Core.ToQuery GetDefaultRetentionPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDefaultRetentionPolicy where
        toHeaders GetDefaultRetentionPolicy{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.GetDefaultRetentionPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDefaultRetentionPolicy where
        toJSON GetDefaultRetentionPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId)])

instance Core.AWSRequest GetDefaultRetentionPolicy where
        type Rs GetDefaultRetentionPolicy =
             GetDefaultRetentionPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDefaultRetentionPolicyResponse' Core.<$>
                   (x Core..:? "Description") Core.<*>
                     x Core..:? "FolderConfigurations"
                     Core.<*> x Core..:? "Id"
                     Core.<*> x Core..:? "Name"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDefaultRetentionPolicyResponse' smart constructor.
data GetDefaultRetentionPolicyResponse = GetDefaultRetentionPolicyResponse'
  { description :: Core.Maybe Core.Text
    -- ^ The retention policy description.
  , folderConfigurations :: Core.Maybe [Types.FolderConfiguration]
    -- ^ The retention policy folder configurations.
  , id :: Core.Maybe Types.ShortString
    -- ^ The retention policy ID.
  , name :: Core.Maybe Types.ShortString
    -- ^ The retention policy name.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultRetentionPolicyResponse' value with any optional fields omitted.
mkGetDefaultRetentionPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDefaultRetentionPolicyResponse
mkGetDefaultRetentionPolicyResponse responseStatus
  = GetDefaultRetentionPolicyResponse'{description = Core.Nothing,
                                       folderConfigurations = Core.Nothing, id = Core.Nothing,
                                       name = Core.Nothing, responseStatus}

-- | The retention policy description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsDescription :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe Core.Text)
gdrprrsDescription = Lens.field @"description"
{-# INLINEABLE gdrprrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The retention policy folder configurations.
--
-- /Note:/ Consider using 'folderConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsFolderConfigurations :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe [Types.FolderConfiguration])
gdrprrsFolderConfigurations = Lens.field @"folderConfigurations"
{-# INLINEABLE gdrprrsFolderConfigurations #-}
{-# DEPRECATED folderConfigurations "Use generic-lens or generic-optics with 'folderConfigurations' instead"  #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsId :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe Types.ShortString)
gdrprrsId = Lens.field @"id"
{-# INLINEABLE gdrprrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The retention policy name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsName :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe Types.ShortString)
gdrprrsName = Lens.field @"name"
{-# INLINEABLE gdrprrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsResponseStatus :: Lens.Lens' GetDefaultRetentionPolicyResponse Core.Int
gdrprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
