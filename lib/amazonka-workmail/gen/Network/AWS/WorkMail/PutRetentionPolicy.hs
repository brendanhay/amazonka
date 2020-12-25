{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutRetentionPolicy (..),
    mkPutRetentionPolicy,

    -- ** Request lenses
    prpOrganizationId,
    prpName,
    prpFolderConfigurations,
    prpDescription,
    prpId,

    -- * Destructuring the response
    PutRetentionPolicyResponse (..),
    mkPutRetentionPolicyResponse,

    -- ** Response lenses
    prprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { -- | The organization ID.
    organizationId :: Types.OrganizationId,
    -- | The retention policy name.
    name :: Types.Name,
    -- | The retention policy folder configurations.
    folderConfigurations :: [Types.FolderConfiguration],
    -- | The retention policy description.
    description :: Core.Maybe Types.PolicyDescription,
    -- | The retention policy ID.
    id :: Core.Maybe Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRetentionPolicy' value with any optional fields omitted.
mkPutRetentionPolicy ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'name'
  Types.Name ->
  PutRetentionPolicy
mkPutRetentionPolicy organizationId name =
  PutRetentionPolicy'
    { organizationId,
      name,
      folderConfigurations = Core.mempty,
      description = Core.Nothing,
      id = Core.Nothing
    }

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpOrganizationId :: Lens.Lens' PutRetentionPolicy Types.OrganizationId
prpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED prpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The retention policy name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpName :: Lens.Lens' PutRetentionPolicy Types.Name
prpName = Lens.field @"name"
{-# DEPRECATED prpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The retention policy folder configurations.
--
-- /Note:/ Consider using 'folderConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpFolderConfigurations :: Lens.Lens' PutRetentionPolicy [Types.FolderConfiguration]
prpFolderConfigurations = Lens.field @"folderConfigurations"
{-# DEPRECATED prpFolderConfigurations "Use generic-lens or generic-optics with 'folderConfigurations' instead." #-}

-- | The retention policy description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpDescription :: Lens.Lens' PutRetentionPolicy (Core.Maybe Types.PolicyDescription)
prpDescription = Lens.field @"description"
{-# DEPRECATED prpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpId :: Lens.Lens' PutRetentionPolicy (Core.Maybe Types.Id)
prpId = Lens.field @"id"
{-# DEPRECATED prpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name),
            Core.Just ("FolderConfigurations" Core..= folderConfigurations),
            ("Description" Core..=) Core.<$> description,
            ("Id" Core..=) Core.<$> id
          ]
      )

instance Core.AWSRequest PutRetentionPolicy where
  type Rs PutRetentionPolicy = PutRetentionPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.PutRetentionPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRetentionPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutRetentionPolicyResponse' smart constructor.
newtype PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutRetentionPolicyResponse' value with any optional fields omitted.
mkPutRetentionPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutRetentionPolicyResponse
mkPutRetentionPolicyResponse responseStatus =
  PutRetentionPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResponseStatus :: Lens.Lens' PutRetentionPolicyResponse Core.Int
prprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
