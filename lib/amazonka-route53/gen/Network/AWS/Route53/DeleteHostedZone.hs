{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- If the hosted zone was created by another service, such as AWS Cloud Map, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DeleteHostedZone.html#delete-public-hosted-zone-created-by-another-service Deleting Public Hosted Zones That Were Created by Another Service> in the /Amazon Route 53 Developer Guide/ for information about how to delete it. (The process is the same for public and private hosted zones that were created by another service.)
--
-- If you want to keep your domain registration but you want to stop routing internet traffic to your website or web application, we recommend that you delete resource record sets in the hosted zone instead of deleting the hosted zone.
--
-- /Important:/ If you delete a hosted zone, you can't undelete it. You must create a new hosted zone and update the name servers for your domain registration, which can require up to 48 hours to take effect. (If you delegated responsibility for a subdomain to a hosted zone and you delete the child hosted zone, you must update the name servers in the parent hosted zone.) In addition, if you delete a hosted zone, someone could hijack the domain and route traffic to their own resources using your domain name.
--
-- If you want to avoid the monthly charge for the hosted zone, you can transfer DNS service for the domain to a free DNS service. When you transfer DNS service, you have to update the name servers for the domain registration. If the domain is registered with Route 53, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_UpdateDomainNameservers.html UpdateDomainNameservers> for information about how to replace Route 53 name servers with name servers for the new DNS service. If the domain is registered with another registrar, use the method provided by the registrar to update name servers for the domain registration. For more information, perform an internet search on "free DNS service."
--
-- You can delete a hosted zone only if it contains only the default SOA record and NS resource record sets. If the hosted zone contains other resource record sets, you must delete them before you can delete the hosted zone. If you try to delete a hosted zone that contains other resource record sets, the request fails, and Route 53 returns a @HostedZoneNotEmpty@ error. For information about deleting records from your hosted zone, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html ChangeResourceRecordSets> .
--
-- To verify that the hosted zone has been deleted, do one of the following:
--
--     * Use the @GetHostedZone@ action to request information about the hosted zone.
--
--     * Use the @ListHostedZones@ action to get a list of the hosted zones associated with the current AWS account.
module Network.AWS.Route53.DeleteHostedZone
  ( -- * Creating a Request
    deleteHostedZone,
    DeleteHostedZone,

    -- * Request Lenses
    dhzId,

    -- * Destructuring the Response
    deleteHostedZoneResponse,
    DeleteHostedZoneResponse,

    -- * Response Lenses
    dhzrsResponseStatus,
    dhzrsChangeInfo,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types

-- | A request to delete a hosted zone.
--
--
--
-- /See:/ 'deleteHostedZone' smart constructor.
newtype DeleteHostedZone = DeleteHostedZone' {_dhzId :: ResourceId}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhzId' - The ID of the hosted zone you want to delete.
deleteHostedZone ::
  -- | 'dhzId'
  ResourceId ->
  DeleteHostedZone
deleteHostedZone pId_ = DeleteHostedZone' {_dhzId = pId_}

-- | The ID of the hosted zone you want to delete.
dhzId :: Lens' DeleteHostedZone ResourceId
dhzId = lens _dhzId (\s a -> s {_dhzId = a})

instance AWSRequest DeleteHostedZone where
  type Rs DeleteHostedZone = DeleteHostedZoneResponse
  request = delete route53
  response =
    receiveXML
      ( \s h x ->
          DeleteHostedZoneResponse'
            <$> (pure (fromEnum s)) <*> (x .@ "ChangeInfo")
      )

instance Hashable DeleteHostedZone

instance NFData DeleteHostedZone

instance ToHeaders DeleteHostedZone where
  toHeaders = const mempty

instance ToPath DeleteHostedZone where
  toPath DeleteHostedZone' {..} =
    mconcat ["/2013-04-01/hostedzone/", toBS _dhzId]

instance ToQuery DeleteHostedZone where
  toQuery = const mempty

-- | A complex type that contains the response to a @DeleteHostedZone@ request.
--
--
--
-- /See:/ 'deleteHostedZoneResponse' smart constructor.
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
  { _dhzrsResponseStatus ::
      !Int,
    _dhzrsChangeInfo :: !ChangeInfo
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhzrsResponseStatus' - -- | The response status code.
--
-- * 'dhzrsChangeInfo' - A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
deleteHostedZoneResponse ::
  -- | 'dhzrsResponseStatus'
  Int ->
  -- | 'dhzrsChangeInfo'
  ChangeInfo ->
  DeleteHostedZoneResponse
deleteHostedZoneResponse pResponseStatus_ pChangeInfo_ =
  DeleteHostedZoneResponse'
    { _dhzrsResponseStatus =
        pResponseStatus_,
      _dhzrsChangeInfo = pChangeInfo_
    }

-- | -- | The response status code.
dhzrsResponseStatus :: Lens' DeleteHostedZoneResponse Int
dhzrsResponseStatus = lens _dhzrsResponseStatus (\s a -> s {_dhzrsResponseStatus = a})

-- | A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
dhzrsChangeInfo :: Lens' DeleteHostedZoneResponse ChangeInfo
dhzrsChangeInfo = lens _dhzrsChangeInfo (\s a -> s {_dhzrsChangeInfo = a})

instance NFData DeleteHostedZoneResponse
