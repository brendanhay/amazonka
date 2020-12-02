{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a hosted zone.
--
--
-- /Important:/ If the name servers for the hosted zone are associated with a domain and if you want to make the domain unavailable on the Internet, we recommend that you delete the name servers from the domain to prevent future DNS queries from possibly being misrouted. If the domain is registered with Amazon Route 53, see @UpdateDomainNameservers@ . If the domain is registered with another registrar, use the method provided by the registrar to delete name servers for the domain.
--
-- Some domain registries don't allow you to remove all of the name servers for a domain. If the registry for your domain requires one or more name servers, we recommend that you delete the hosted zone only if you transfer DNS service to another service provider, and you replace the name servers for the domain with name servers from the new provider.
--
-- You can delete a hosted zone only if it contains only the default SOA record and NS resource record sets. If the hosted zone contains other resource record sets, you must delete them before you can delete the hosted zone. If you try to delete a hosted zone that contains other resource record sets, the request fails, and Amazon Route 53 returns a @HostedZoneNotEmpty@ error. For information about deleting records from your hosted zone, see 'ChangeResourceRecordSets' .
--
-- To verify that the hosted zone has been deleted, do one of the following:
--
--     * Use the @GetHostedZone@ action to request information about the hosted zone.
--
--     * Use the @ListHostedZones@ action to get a list of the hosted zones associated with the current AWS account.
--
--
--
module Network.AWS.Route53.DeleteHostedZone
    (
    -- * Creating a Request
      deleteHostedZone
    , DeleteHostedZone
    -- * Request Lenses
    , dhzId

    -- * Destructuring the Response
    , deleteHostedZoneResponse
    , DeleteHostedZoneResponse
    -- * Response Lenses
    , dhzrsResponseStatus
    , dhzrsChangeInfo
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to delete a hosted zone.
--
--
--
-- /See:/ 'deleteHostedZone' smart constructor.
newtype DeleteHostedZone = DeleteHostedZone'
  { _dhzId :: ResourceId
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhzId' - The ID of the hosted zone you want to delete.
deleteHostedZone
    :: ResourceId -- ^ 'dhzId'
    -> DeleteHostedZone
deleteHostedZone pId_ = DeleteHostedZone' {_dhzId = pId_}


-- | The ID of the hosted zone you want to delete.
dhzId :: Lens' DeleteHostedZone ResourceId
dhzId = lens _dhzId (\ s a -> s{_dhzId = a})

instance AWSRequest DeleteHostedZone where
        type Rs DeleteHostedZone = DeleteHostedZoneResponse
        request = delete route53
        response
          = receiveXML
              (\ s h x ->
                 DeleteHostedZoneResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance Hashable DeleteHostedZone where

instance NFData DeleteHostedZone where

instance ToHeaders DeleteHostedZone where
        toHeaders = const mempty

instance ToPath DeleteHostedZone where
        toPath DeleteHostedZone'{..}
          = mconcat ["/2013-04-01/hostedzone/", toBS _dhzId]

instance ToQuery DeleteHostedZone where
        toQuery = const mempty

-- | A complex type that contains the response to a @DeleteHostedZone@ request.
--
--
--
-- /See:/ 'deleteHostedZoneResponse' smart constructor.
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
  { _dhzrsResponseStatus :: !Int
  , _dhzrsChangeInfo     :: !ChangeInfo
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhzrsResponseStatus' - -- | The response status code.
--
-- * 'dhzrsChangeInfo' - A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
deleteHostedZoneResponse
    :: Int -- ^ 'dhzrsResponseStatus'
    -> ChangeInfo -- ^ 'dhzrsChangeInfo'
    -> DeleteHostedZoneResponse
deleteHostedZoneResponse pResponseStatus_ pChangeInfo_ =
  DeleteHostedZoneResponse'
    {_dhzrsResponseStatus = pResponseStatus_, _dhzrsChangeInfo = pChangeInfo_}


-- | -- | The response status code.
dhzrsResponseStatus :: Lens' DeleteHostedZoneResponse Int
dhzrsResponseStatus = lens _dhzrsResponseStatus (\ s a -> s{_dhzrsResponseStatus = a})

-- | A complex type that contains the ID, the status, and the date and time of a request to delete a hosted zone.
dhzrsChangeInfo :: Lens' DeleteHostedZoneResponse ChangeInfo
dhzrsChangeInfo = lens _dhzrsChangeInfo (\ s a -> s{_dhzrsChangeInfo = a})

instance NFData DeleteHostedZoneResponse where
