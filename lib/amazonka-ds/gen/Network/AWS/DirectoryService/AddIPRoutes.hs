{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddIPRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the DNS server for your on-premises domain uses a publicly addressable IP address, you must add a CIDR address block to correctly route traffic to and from your Microsoft AD on Amazon Web Services. /AddIpRoutes/ adds this address block. You can also use /AddIpRoutes/ to facilitate routing traffic that uses public IP ranges from your Microsoft AD on AWS to a peer VPC.
--
-- Before you call /AddIpRoutes/ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the /AddIpRoutes/ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.AddIPRoutes
  ( -- * Creating a request
    AddIPRoutes (..),
    mkAddIPRoutes,

    -- ** Request lenses
    airUpdateSecurityGroupForDirectoryControllers,
    airDirectoryId,
    airIPRoutes,

    -- * Destructuring the response
    AddIPRoutesResponse (..),
    mkAddIPRoutesResponse,

    -- ** Response lenses
    airrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddIPRoutes' smart constructor.
data AddIPRoutes = AddIPRoutes'
  { updateSecurityGroupForDirectoryControllers ::
      Lude.Maybe Lude.Bool,
    directoryId :: Lude.Text,
    ipRoutes :: [IPRoute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddIPRoutes' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier (ID) of the directory to which to add the address block.
-- * 'ipRoutes' - IP address blocks, using CIDR format, of the traffic to route. This is often the IP address block of the DNS server used for your on-premises domain.
-- * 'updateSecurityGroupForDirectoryControllers' - If set to true, updates the inbound and outbound rules of the security group that has the description: "AWS created security group for /directory ID/ directory controllers." Following are the new rules:
--
-- Inbound:
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source: 0.0.0.0/0
--
--
--     * Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: All ICMP, Protocol: All, Range: N/A, Source: 0.0.0.0/0
--
--
--
-- Outbound:
--
--     * Type: All traffic, Protocol: All, Range: All, Destination: 0.0.0.0/0
--
--
-- These security rules impact an internal network interface that is not exposed publicly.
mkAddIPRoutes ::
  -- | 'directoryId'
  Lude.Text ->
  AddIPRoutes
mkAddIPRoutes pDirectoryId_ =
  AddIPRoutes'
    { updateSecurityGroupForDirectoryControllers =
        Lude.Nothing,
      directoryId = pDirectoryId_,
      ipRoutes = Lude.mempty
    }

-- | If set to true, updates the inbound and outbound rules of the security group that has the description: "AWS created security group for /directory ID/ directory controllers." Following are the new rules:
--
-- Inbound:
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 123, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 138, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom UDP Rule, Protocol: UDP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 88, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 135, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 445, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 464, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 636, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 1024-65535, Source: 0.0.0.0/0
--
--
--     * Type: Custom TCP Rule, Protocol: TCP, Range: 3268-33269, Source: 0.0.0.0/0
--
--
--     * Type: DNS (UDP), Protocol: UDP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: DNS (TCP), Protocol: TCP, Range: 53, Source: 0.0.0.0/0
--
--
--     * Type: LDAP, Protocol: TCP, Range: 389, Source: 0.0.0.0/0
--
--
--     * Type: All ICMP, Protocol: All, Range: N/A, Source: 0.0.0.0/0
--
--
--
-- Outbound:
--
--     * Type: All traffic, Protocol: All, Range: All, Destination: 0.0.0.0/0
--
--
-- These security rules impact an internal network interface that is not exposed publicly.
--
-- /Note:/ Consider using 'updateSecurityGroupForDirectoryControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airUpdateSecurityGroupForDirectoryControllers :: Lens.Lens' AddIPRoutes (Lude.Maybe Lude.Bool)
airUpdateSecurityGroupForDirectoryControllers = Lens.lens (updateSecurityGroupForDirectoryControllers :: AddIPRoutes -> Lude.Maybe Lude.Bool) (\s a -> s {updateSecurityGroupForDirectoryControllers = a} :: AddIPRoutes)
{-# DEPRECATED airUpdateSecurityGroupForDirectoryControllers "Use generic-lens or generic-optics with 'updateSecurityGroupForDirectoryControllers' instead." #-}

-- | Identifier (ID) of the directory to which to add the address block.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airDirectoryId :: Lens.Lens' AddIPRoutes Lude.Text
airDirectoryId = Lens.lens (directoryId :: AddIPRoutes -> Lude.Text) (\s a -> s {directoryId = a} :: AddIPRoutes)
{-# DEPRECATED airDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | IP address blocks, using CIDR format, of the traffic to route. This is often the IP address block of the DNS server used for your on-premises domain.
--
-- /Note:/ Consider using 'ipRoutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airIPRoutes :: Lens.Lens' AddIPRoutes [IPRoute]
airIPRoutes = Lens.lens (ipRoutes :: AddIPRoutes -> [IPRoute]) (\s a -> s {ipRoutes = a} :: AddIPRoutes)
{-# DEPRECATED airIPRoutes "Use generic-lens or generic-optics with 'ipRoutes' instead." #-}

instance Lude.AWSRequest AddIPRoutes where
  type Rs AddIPRoutes = AddIPRoutesResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddIPRoutesResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddIPRoutes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.AddIpRoutes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddIPRoutes where
  toJSON AddIPRoutes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UpdateSecurityGroupForDirectoryControllers" Lude..=)
              Lude.<$> updateSecurityGroupForDirectoryControllers,
            Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("IpRoutes" Lude..= ipRoutes)
          ]
      )

instance Lude.ToPath AddIPRoutes where
  toPath = Lude.const "/"

instance Lude.ToQuery AddIPRoutes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddIPRoutesResponse' smart constructor.
newtype AddIPRoutesResponse = AddIPRoutesResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddIPRoutesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddIPRoutesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddIPRoutesResponse
mkAddIPRoutesResponse pResponseStatus_ =
  AddIPRoutesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airrsResponseStatus :: Lens.Lens' AddIPRoutesResponse Lude.Int
airrsResponseStatus = Lens.lens (responseStatus :: AddIPRoutesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddIPRoutesResponse)
{-# DEPRECATED airrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
