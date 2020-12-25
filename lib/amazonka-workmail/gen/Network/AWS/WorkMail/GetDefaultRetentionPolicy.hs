{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetDefaultRetentionPolicy (..),
    mkGetDefaultRetentionPolicy,

    -- ** Request lenses
    gdrpOrganizationId,

    -- * Destructuring the response
    GetDefaultRetentionPolicyResponse (..),
    mkGetDefaultRetentionPolicyResponse,

    -- ** Response lenses
    gdrprrsDescription,
    gdrprrsFolderConfigurations,
    gdrprrsId,
    gdrprrsName,
    gdrprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkGetDefaultRetentionPolicy' smart constructor.
newtype GetDefaultRetentionPolicy = GetDefaultRetentionPolicy'
  { -- | The organization ID.
    organizationId :: Types.OrganizationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultRetentionPolicy' value with any optional fields omitted.
mkGetDefaultRetentionPolicy ::
  -- | 'organizationId'
  Types.OrganizationId ->
  GetDefaultRetentionPolicy
mkGetDefaultRetentionPolicy organizationId =
  GetDefaultRetentionPolicy' {organizationId}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrpOrganizationId :: Lens.Lens' GetDefaultRetentionPolicy Types.OrganizationId
gdrpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED gdrpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Core.FromJSON GetDefaultRetentionPolicy where
  toJSON GetDefaultRetentionPolicy {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("OrganizationId" Core..= organizationId)]
      )

instance Core.AWSRequest GetDefaultRetentionPolicy where
  type
    Rs GetDefaultRetentionPolicy =
      GetDefaultRetentionPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkMailService.GetDefaultRetentionPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDefaultRetentionPolicyResponse'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..:? "FolderConfigurations")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDefaultRetentionPolicyResponse' smart constructor.
data GetDefaultRetentionPolicyResponse = GetDefaultRetentionPolicyResponse'
  { -- | The retention policy description.
    description :: Core.Maybe Types.String,
    -- | The retention policy folder configurations.
    folderConfigurations :: Core.Maybe [Types.FolderConfiguration],
    -- | The retention policy ID.
    id :: Core.Maybe Types.ShortString,
    -- | The retention policy name.
    name :: Core.Maybe Types.ShortString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDefaultRetentionPolicyResponse' value with any optional fields omitted.
mkGetDefaultRetentionPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDefaultRetentionPolicyResponse
mkGetDefaultRetentionPolicyResponse responseStatus =
  GetDefaultRetentionPolicyResponse'
    { description = Core.Nothing,
      folderConfigurations = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The retention policy description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsDescription :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe Types.String)
gdrprrsDescription = Lens.field @"description"
{-# DEPRECATED gdrprrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The retention policy folder configurations.
--
-- /Note:/ Consider using 'folderConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsFolderConfigurations :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe [Types.FolderConfiguration])
gdrprrsFolderConfigurations = Lens.field @"folderConfigurations"
{-# DEPRECATED gdrprrsFolderConfigurations "Use generic-lens or generic-optics with 'folderConfigurations' instead." #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsId :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe Types.ShortString)
gdrprrsId = Lens.field @"id"
{-# DEPRECATED gdrprrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The retention policy name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsName :: Lens.Lens' GetDefaultRetentionPolicyResponse (Core.Maybe Types.ShortString)
gdrprrsName = Lens.field @"name"
{-# DEPRECATED gdrprrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsResponseStatus :: Lens.Lens' GetDefaultRetentionPolicyResponse Core.Int
gdrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
