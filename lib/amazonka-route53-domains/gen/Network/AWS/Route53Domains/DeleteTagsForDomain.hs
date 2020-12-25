{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DeleteTagsForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the specified tags for a domain.
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
module Network.AWS.Route53Domains.DeleteTagsForDomain
  ( -- * Creating a request
    DeleteTagsForDomain (..),
    mkDeleteTagsForDomain,

    -- ** Request lenses
    dtfdDomainName,
    dtfdTagsToDelete,

    -- * Destructuring the response
    DeleteTagsForDomainResponse (..),
    mkDeleteTagsForDomainResponse,

    -- ** Response lenses
    dtfdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The DeleteTagsForDomainRequest includes the following elements.
--
-- /See:/ 'mkDeleteTagsForDomain' smart constructor.
data DeleteTagsForDomain = DeleteTagsForDomain'
  { -- | The domain for which you want to delete one or more tags.
    domainName :: Types.DomainName,
    -- | A list of tag keys to delete.
    tagsToDelete :: [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsForDomain' value with any optional fields omitted.
mkDeleteTagsForDomain ::
  -- | 'domainName'
  Types.DomainName ->
  DeleteTagsForDomain
mkDeleteTagsForDomain domainName =
  DeleteTagsForDomain' {domainName, tagsToDelete = Core.mempty}

-- | The domain for which you want to delete one or more tags.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfdDomainName :: Lens.Lens' DeleteTagsForDomain Types.DomainName
dtfdDomainName = Lens.field @"domainName"
{-# DEPRECATED dtfdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A list of tag keys to delete.
--
-- /Note:/ Consider using 'tagsToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfdTagsToDelete :: Lens.Lens' DeleteTagsForDomain [Types.TagKey]
dtfdTagsToDelete = Lens.field @"tagsToDelete"
{-# DEPRECATED dtfdTagsToDelete "Use generic-lens or generic-optics with 'tagsToDelete' instead." #-}

instance Core.FromJSON DeleteTagsForDomain where
  toJSON DeleteTagsForDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            Core.Just ("TagsToDelete" Core..= tagsToDelete)
          ]
      )

instance Core.AWSRequest DeleteTagsForDomain where
  type Rs DeleteTagsForDomain = DeleteTagsForDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53Domains_v20140515.DeleteTagsForDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagsForDomainResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTagsForDomainResponse' smart constructor.
newtype DeleteTagsForDomainResponse = DeleteTagsForDomainResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagsForDomainResponse' value with any optional fields omitted.
mkDeleteTagsForDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTagsForDomainResponse
mkDeleteTagsForDomainResponse responseStatus =
  DeleteTagsForDomainResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfdrrsResponseStatus :: Lens.Lens' DeleteTagsForDomainResponse Core.Int
dtfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
