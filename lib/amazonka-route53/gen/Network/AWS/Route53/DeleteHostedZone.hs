{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a hosted zone.
--
-- If the hosted zone was created by another service, such as AWS Cloud Map, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DeleteHostedZone.html#delete-public-hosted-zone-created-by-another-service Deleting Public Hosted Zones That Were Created by Another Service> in the /Amazon Route 53 Developer Guide/ for information about how to delete it. (The process is the same for public and private hosted zones that were created by another service.)
-- If you want to keep your domain registration but you want to stop routing internet traffic to your website or web application, we recommend that you delete resource record sets in the hosted zone instead of deleting the hosted zone.
-- /Important:/ If you delete a hosted zone, you can't undelete it. You must create a new hosted zone and update the name servers for your domain registration, which can require up to 48 hours to take effect. (If you delegated responsibility for a subdomain to a hosted zone and you delete the child hosted zone, you must update the name servers in the parent hosted zone.) In addition, if you delete a hosted zone, someone could hijack the domain and route traffic to their own resources using your domain name.
-- If you want to avoid the monthly charge for the hosted zone, you can transfer DNS service for the domain to a free DNS service. When you transfer DNS service, you have to update the name servers for the domain registration. If the domain is registered with Route 53, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_UpdateDomainNameservers.html UpdateDomainNameservers> for information about how to replace Route 53 name servers with name servers for the new DNS service. If the domain is registered with another registrar, use the method provided by the registrar to update name servers for the domain registration. For more information, perform an internet search on "free DNS service."
-- You can delete a hosted zone only if it contains only the default SOA record and NS resource record sets. If the hosted zone contains other resource record sets, you must delete them before you can delete the hosted zone. If you try to delete a hosted zone that contains other resource record sets, the request fails, and Route 53 returns a @HostedZoneNotEmpty@ error. For information about deleting records from your hosted zone, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html ChangeResourceRecordSets> .
-- To verify that the hosted zone has been deleted, do one of the following:
--
--     * Use the @GetHostedZone@ action to request information about the hosted zone.
--
--
--     * Use the @ListHostedZones@ action to get a list of the hosted zones associated with the current AWS account.
--
--
module Network.AWS.Route53.DeleteHostedZone
    (
    -- * Creating a request
      DeleteHostedZone (..)
    , mkDeleteHostedZone
    -- ** Request lenses
    , dhzId

    -- * Destructuring the response
    , DeleteHostedZoneResponse (..)
    , mkDeleteHostedZoneResponse
    -- ** Response lenses
    , dhzrrsChangeInfo
    , dhzrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to delete a hosted zone.
--
-- /See:/ 'mkDeleteHostedZone' smart constructor.
newtype DeleteHostedZone = DeleteHostedZone'
  { id :: Types.ResourceId
    -- ^ The ID of the hosted zone you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHostedZone' value with any optional fields omitted.
mkDeleteHostedZone
    :: Types.ResourceId -- ^ 'id'
    -> DeleteHostedZone
mkDeleteHostedZone id = DeleteHostedZone'{id}

-- | The ID of the hosted zone you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhzId :: Lens.Lens' DeleteHostedZone Types.ResourceId
dhzId = Lens.field @"id"
{-# INLINEABLE dhzId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteHostedZone where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteHostedZone where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteHostedZone where
        type Rs DeleteHostedZone = DeleteHostedZoneResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2013-04-01/hostedzone/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DeleteHostedZoneResponse' Core.<$>
                   (x Core..@ "ChangeInfo") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response to a @DeleteHostedZone@ request.
--
-- /See:/ 'mkDeleteHostedZoneResponse' smart constructor.
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
  { changeInfo :: Types.ChangeInfo
    -- ^ A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteHostedZoneResponse' value with any optional fields omitted.
mkDeleteHostedZoneResponse
    :: Types.ChangeInfo -- ^ 'changeInfo'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteHostedZoneResponse
mkDeleteHostedZoneResponse changeInfo responseStatus
  = DeleteHostedZoneResponse'{changeInfo, responseStatus}

-- | A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhzrrsChangeInfo :: Lens.Lens' DeleteHostedZoneResponse Types.ChangeInfo
dhzrrsChangeInfo = Lens.field @"changeInfo"
{-# INLINEABLE dhzrrsChangeInfo #-}
{-# DEPRECATED changeInfo "Use generic-lens or generic-optics with 'changeInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhzrrsResponseStatus :: Lens.Lens' DeleteHostedZoneResponse Core.Int
dhzrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhzrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
