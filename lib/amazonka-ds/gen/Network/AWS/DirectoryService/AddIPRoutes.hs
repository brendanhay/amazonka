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
-- Module      : Network.AWS.DirectoryService.AddIPRoutes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the DNS server for your on-premises domain uses a publicly addressable IP address, you must add a CIDR address block to correctly route traffic to and from your Microsoft AD on Amazon Web Services. /AddIpRoutes/ adds this address block. You can also use /AddIpRoutes/ to facilitate routing traffic that uses public IP ranges from your Microsoft AD on AWS to a peer VPC.
--
--
-- Before you call /AddIpRoutes/ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the /AddIpRoutes/ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
--
module Network.AWS.DirectoryService.AddIPRoutes
    (
    -- * Creating a Request
      addIPRoutes
    , AddIPRoutes
    -- * Request Lenses
    , airUpdateSecurityGroupForDirectoryControllers
    , airDirectoryId
    , airIPRoutes

    -- * Destructuring the Response
    , addIPRoutesResponse
    , AddIPRoutesResponse
    -- * Response Lenses
    , airrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addIPRoutes' smart constructor.
data AddIPRoutes = AddIPRoutes'
  { _airUpdateSecurityGroupForDirectoryControllers :: !(Maybe Bool)
  , _airDirectoryId                                :: !Text
  , _airIPRoutes                                   :: ![IPRoute]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddIPRoutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airUpdateSecurityGroupForDirectoryControllers' - If set to true, updates the inbound and outbound rules of the security group that has the description: "AWS created security group for /directory ID/ directory controllers." Following are the new rules:  Inbound:     * Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source: 0.0.0.0/0     * Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0/0     * Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0/0     * Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0/0     * Type: All ICMP, Protocol: All, Range: N/A, Source: 0.0.0.0/0 Outbound:     * Type: All traffic, Protocol: All, Range: All, Destination: 0.0.0.0/0 These security rules impact an internal network interface that is not exposed publicly.
--
-- * 'airDirectoryId' - Identifier (ID) of the directory to which to add the address block.
--
-- * 'airIPRoutes' - IP address blocks, using CIDR format, of the traffic to route. This is often the IP address block of the DNS server used for your on-premises domain.
addIPRoutes
    :: Text -- ^ 'airDirectoryId'
    -> AddIPRoutes
addIPRoutes pDirectoryId_ =
  AddIPRoutes'
    { _airUpdateSecurityGroupForDirectoryControllers = Nothing
    , _airDirectoryId = pDirectoryId_
    , _airIPRoutes = mempty
    }


-- | If set to true, updates the inbound and outbound rules of the security group that has the description: "AWS created security group for /directory ID/ directory controllers." Following are the new rules:  Inbound:     * Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0/0     * Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source: 0.0.0.0/0     * Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source: 0.0.0.0/0     * Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0/0     * Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0/0     * Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0/0     * Type: All ICMP, Protocol: All, Range: N/A, Source: 0.0.0.0/0 Outbound:     * Type: All traffic, Protocol: All, Range: All, Destination: 0.0.0.0/0 These security rules impact an internal network interface that is not exposed publicly.
airUpdateSecurityGroupForDirectoryControllers :: Lens' AddIPRoutes (Maybe Bool)
airUpdateSecurityGroupForDirectoryControllers = lens _airUpdateSecurityGroupForDirectoryControllers (\ s a -> s{_airUpdateSecurityGroupForDirectoryControllers = a})

-- | Identifier (ID) of the directory to which to add the address block.
airDirectoryId :: Lens' AddIPRoutes Text
airDirectoryId = lens _airDirectoryId (\ s a -> s{_airDirectoryId = a})

-- | IP address blocks, using CIDR format, of the traffic to route. This is often the IP address block of the DNS server used for your on-premises domain.
airIPRoutes :: Lens' AddIPRoutes [IPRoute]
airIPRoutes = lens _airIPRoutes (\ s a -> s{_airIPRoutes = a}) . _Coerce

instance AWSRequest AddIPRoutes where
        type Rs AddIPRoutes = AddIPRoutesResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 AddIPRoutesResponse' <$> (pure (fromEnum s)))

instance Hashable AddIPRoutes where

instance NFData AddIPRoutes where

instance ToHeaders AddIPRoutes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.AddIpRoutes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddIPRoutes where
        toJSON AddIPRoutes'{..}
          = object
              (catMaybes
                 [("UpdateSecurityGroupForDirectoryControllers" .=)
                    <$> _airUpdateSecurityGroupForDirectoryControllers,
                  Just ("DirectoryId" .= _airDirectoryId),
                  Just ("IpRoutes" .= _airIPRoutes)])

instance ToPath AddIPRoutes where
        toPath = const "/"

instance ToQuery AddIPRoutes where
        toQuery = const mempty

-- | /See:/ 'addIPRoutesResponse' smart constructor.
newtype AddIPRoutesResponse = AddIPRoutesResponse'
  { _airrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddIPRoutesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'airrsResponseStatus' - -- | The response status code.
addIPRoutesResponse
    :: Int -- ^ 'airrsResponseStatus'
    -> AddIPRoutesResponse
addIPRoutesResponse pResponseStatus_ =
  AddIPRoutesResponse' {_airrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
airrsResponseStatus :: Lens' AddIPRoutesResponse Int
airrsResponseStatus = lens _airrsResponseStatus (\ s a -> s{_airrsResponseStatus = a})

instance NFData AddIPRoutesResponse where
