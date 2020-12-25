{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a search domain and all of its data. Once a domain has been deleted, it cannot be recovered. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/deleting-domains.html Deleting a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteDomain
  ( -- * Creating a request
    DeleteDomain (..),
    mkDeleteDomain,

    -- ** Request lenses
    ddDomainName,

    -- * Destructuring the response
    DeleteDomainResponse (..),
    mkDeleteDomainResponse,

    -- ** Response lenses
    ddrrsDomainStatus,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteDomain' @ operation. Specifies the name of the domain you want to delete.
--
-- /See:/ 'mkDeleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain'
  { -- | The name of the domain you want to permanently delete.
    domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomain' value with any optional fields omitted.
mkDeleteDomain ::
  -- | 'domainName'
  Types.DomainName ->
  DeleteDomain
mkDeleteDomain domainName = DeleteDomain' {domainName}

-- | The name of the domain you want to permanently delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DeleteDomain Types.DomainName
ddDomainName = Lens.field @"domainName"
{-# DEPRECATED ddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteDomain")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteDomainResult"
      ( \s h x ->
          DeleteDomainResponse'
            Core.<$> (x Core..@? "DomainStatus") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @DeleteDomain@ request. Contains the status of a newly deleted domain, or no status if the domain has already been completely deleted.
--
-- /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { domainStatus :: Core.Maybe Types.DomainStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainResponse' value with any optional fields omitted.
mkDeleteDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDomainResponse
mkDeleteDomainResponse responseStatus =
  DeleteDomainResponse'
    { domainStatus = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainStatus :: Lens.Lens' DeleteDomainResponse (Core.Maybe Types.DomainStatus)
ddrrsDomainStatus = Lens.field @"domainStatus"
{-# DEPRECATED ddrrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDomainResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
