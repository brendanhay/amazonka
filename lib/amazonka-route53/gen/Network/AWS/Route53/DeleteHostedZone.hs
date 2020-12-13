{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.Route53.DeleteHostedZone
  ( -- * Creating a request
    DeleteHostedZone (..),
    mkDeleteHostedZone,

    -- ** Request lenses
    dhzId,

    -- * Destructuring the response
    DeleteHostedZoneResponse (..),
    mkDeleteHostedZoneResponse,

    -- ** Response lenses
    dhzrsChangeInfo,
    dhzrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to delete a hosted zone.
--
-- /See:/ 'mkDeleteHostedZone' smart constructor.
newtype DeleteHostedZone = DeleteHostedZone'
  { -- | The ID of the hosted zone you want to delete.
    id :: ResourceId
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHostedZone' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the hosted zone you want to delete.
mkDeleteHostedZone ::
  -- | 'id'
  ResourceId ->
  DeleteHostedZone
mkDeleteHostedZone pId_ = DeleteHostedZone' {id = pId_}

-- | The ID of the hosted zone you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhzId :: Lens.Lens' DeleteHostedZone ResourceId
dhzId = Lens.lens (id :: DeleteHostedZone -> ResourceId) (\s a -> s {id = a} :: DeleteHostedZone)
{-# DEPRECATED dhzId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteHostedZone where
  type Rs DeleteHostedZone = DeleteHostedZoneResponse
  request = Req.delete route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteHostedZoneResponse'
            Lude.<$> (x Lude..@ "ChangeInfo") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteHostedZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteHostedZone where
  toPath DeleteHostedZone' {..} =
    Lude.mconcat ["/2013-04-01/hostedzone/", Lude.toBS id]

instance Lude.ToQuery DeleteHostedZone where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response to a @DeleteHostedZone@ request.
--
-- /See:/ 'mkDeleteHostedZoneResponse' smart constructor.
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
  { -- | A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
    changeInfo :: ChangeInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHostedZoneResponse' with the minimum fields required to make a request.
--
-- * 'changeInfo' - A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
-- * 'responseStatus' - The response status code.
mkDeleteHostedZoneResponse ::
  -- | 'changeInfo'
  ChangeInfo ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteHostedZoneResponse
mkDeleteHostedZoneResponse pChangeInfo_ pResponseStatus_ =
  DeleteHostedZoneResponse'
    { changeInfo = pChangeInfo_,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhzrsChangeInfo :: Lens.Lens' DeleteHostedZoneResponse ChangeInfo
dhzrsChangeInfo = Lens.lens (changeInfo :: DeleteHostedZoneResponse -> ChangeInfo) (\s a -> s {changeInfo = a} :: DeleteHostedZoneResponse)
{-# DEPRECATED dhzrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhzrsResponseStatus :: Lens.Lens' DeleteHostedZoneResponse Lude.Int
dhzrsResponseStatus = Lens.lens (responseStatus :: DeleteHostedZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteHostedZoneResponse)
{-# DEPRECATED dhzrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
