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
-- Module      : Network.AWS.EC2.DeleteVPCEndpointServiceConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint service configurations in your account. Before you delete the endpoint service configuration, you must reject any @Available@ or @PendingAcceptance@ interface endpoint connections that are attached to the service.
--
--
module Network.AWS.EC2.DeleteVPCEndpointServiceConfigurations
    (
    -- * Creating a Request
      deleteVPCEndpointServiceConfigurations
    , DeleteVPCEndpointServiceConfigurations
    -- * Request Lenses
    , dvpcescDryRun
    , dvpcescServiceIds

    -- * Destructuring the Response
    , deleteVPCEndpointServiceConfigurationsResponse
    , DeleteVPCEndpointServiceConfigurationsResponse
    -- * Response Lenses
    , dvpcescrsUnsuccessful
    , dvpcescrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteVPCEndpointServiceConfigurations' smart constructor.
data DeleteVPCEndpointServiceConfigurations = DeleteVPCEndpointServiceConfigurations'
  { _dvpcescDryRun     :: !(Maybe Bool)
  , _dvpcescServiceIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCEndpointServiceConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcescDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpcescServiceIds' - The IDs of one or more services.
deleteVPCEndpointServiceConfigurations
    :: DeleteVPCEndpointServiceConfigurations
deleteVPCEndpointServiceConfigurations =
  DeleteVPCEndpointServiceConfigurations'
    {_dvpcescDryRun = Nothing, _dvpcescServiceIds = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpcescDryRun :: Lens' DeleteVPCEndpointServiceConfigurations (Maybe Bool)
dvpcescDryRun = lens _dvpcescDryRun (\ s a -> s{_dvpcescDryRun = a})

-- | The IDs of one or more services.
dvpcescServiceIds :: Lens' DeleteVPCEndpointServiceConfigurations [Text]
dvpcescServiceIds = lens _dvpcescServiceIds (\ s a -> s{_dvpcescServiceIds = a}) . _Coerce

instance AWSRequest
           DeleteVPCEndpointServiceConfigurations
         where
        type Rs DeleteVPCEndpointServiceConfigurations =
             DeleteVPCEndpointServiceConfigurationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteVPCEndpointServiceConfigurationsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DeleteVPCEndpointServiceConfigurations
         where

instance NFData
           DeleteVPCEndpointServiceConfigurations
         where

instance ToHeaders
           DeleteVPCEndpointServiceConfigurations
         where
        toHeaders = const mempty

instance ToPath
           DeleteVPCEndpointServiceConfigurations
         where
        toPath = const "/"

instance ToQuery
           DeleteVPCEndpointServiceConfigurations
         where
        toQuery DeleteVPCEndpointServiceConfigurations'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVpcEndpointServiceConfigurations" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvpcescDryRun,
               toQueryList "ServiceId" _dvpcescServiceIds]

-- | /See:/ 'deleteVPCEndpointServiceConfigurationsResponse' smart constructor.
data DeleteVPCEndpointServiceConfigurationsResponse = DeleteVPCEndpointServiceConfigurationsResponse'
  { _dvpcescrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
  , _dvpcescrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCEndpointServiceConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcescrsUnsuccessful' - Information about the service configurations that were not deleted, if applicable.
--
-- * 'dvpcescrsResponseStatus' - -- | The response status code.
deleteVPCEndpointServiceConfigurationsResponse
    :: Int -- ^ 'dvpcescrsResponseStatus'
    -> DeleteVPCEndpointServiceConfigurationsResponse
deleteVPCEndpointServiceConfigurationsResponse pResponseStatus_ =
  DeleteVPCEndpointServiceConfigurationsResponse'
    { _dvpcescrsUnsuccessful = Nothing
    , _dvpcescrsResponseStatus = pResponseStatus_
    }


-- | Information about the service configurations that were not deleted, if applicable.
dvpcescrsUnsuccessful :: Lens' DeleteVPCEndpointServiceConfigurationsResponse [UnsuccessfulItem]
dvpcescrsUnsuccessful = lens _dvpcescrsUnsuccessful (\ s a -> s{_dvpcescrsUnsuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
dvpcescrsResponseStatus :: Lens' DeleteVPCEndpointServiceConfigurationsResponse Int
dvpcescrsResponseStatus = lens _dvpcescrsResponseStatus (\ s a -> s{_dvpcescrsResponseStatus = a})

instance NFData
           DeleteVPCEndpointServiceConfigurationsResponse
         where
