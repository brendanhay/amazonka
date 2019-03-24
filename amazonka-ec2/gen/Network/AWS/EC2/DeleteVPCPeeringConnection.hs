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
-- Module      : Network.AWS.EC2.DeleteVPCPeeringConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a VPC peering connection. Either the owner of the requester VPC or the owner of the accepter VPC can delete the VPC peering connection if it's in the @active@ state. The owner of the requester VPC can delete a VPC peering connection in the @pending-acceptance@ state. You cannot delete a VPC peering connection that's in the @failed@ state.
--
--
module Network.AWS.EC2.DeleteVPCPeeringConnection
    (
    -- * Creating a Request
      deleteVPCPeeringConnection
    , DeleteVPCPeeringConnection
    -- * Request Lenses
    , dvpcDryRun
    , dvpcVPCPeeringConnectionId

    -- * Destructuring the Response
    , deleteVPCPeeringConnectionResponse
    , DeleteVPCPeeringConnectionResponse
    -- * Response Lenses
    , dvpcrsReturn
    , dvpcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteVPCPeeringConnection' smart constructor.
data DeleteVPCPeeringConnection = DeleteVPCPeeringConnection'
  { _dvpcDryRun                 :: !(Maybe Bool)
  , _dvpcVPCPeeringConnectionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpcVPCPeeringConnectionId' - The ID of the VPC peering connection.
deleteVPCPeeringConnection
    :: Text -- ^ 'dvpcVPCPeeringConnectionId'
    -> DeleteVPCPeeringConnection
deleteVPCPeeringConnection pVPCPeeringConnectionId_ =
  DeleteVPCPeeringConnection'
    { _dvpcDryRun = Nothing
    , _dvpcVPCPeeringConnectionId = pVPCPeeringConnectionId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpcDryRun :: Lens' DeleteVPCPeeringConnection (Maybe Bool)
dvpcDryRun = lens _dvpcDryRun (\ s a -> s{_dvpcDryRun = a})

-- | The ID of the VPC peering connection.
dvpcVPCPeeringConnectionId :: Lens' DeleteVPCPeeringConnection Text
dvpcVPCPeeringConnectionId = lens _dvpcVPCPeeringConnectionId (\ s a -> s{_dvpcVPCPeeringConnectionId = a})

instance AWSRequest DeleteVPCPeeringConnection where
        type Rs DeleteVPCPeeringConnection =
             DeleteVPCPeeringConnectionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteVPCPeeringConnectionResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable DeleteVPCPeeringConnection where

instance NFData DeleteVPCPeeringConnection where

instance ToHeaders DeleteVPCPeeringConnection where
        toHeaders = const mempty

instance ToPath DeleteVPCPeeringConnection where
        toPath = const "/"

instance ToQuery DeleteVPCPeeringConnection where
        toQuery DeleteVPCPeeringConnection'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVpcPeeringConnection" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvpcDryRun,
               "VpcPeeringConnectionId" =:
                 _dvpcVPCPeeringConnectionId]

-- | /See:/ 'deleteVPCPeeringConnectionResponse' smart constructor.
data DeleteVPCPeeringConnectionResponse = DeleteVPCPeeringConnectionResponse'
  { _dvpcrsReturn         :: !(Maybe Bool)
  , _dvpcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'dvpcrsResponseStatus' - -- | The response status code.
deleteVPCPeeringConnectionResponse
    :: Int -- ^ 'dvpcrsResponseStatus'
    -> DeleteVPCPeeringConnectionResponse
deleteVPCPeeringConnectionResponse pResponseStatus_ =
  DeleteVPCPeeringConnectionResponse'
    {_dvpcrsReturn = Nothing, _dvpcrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
dvpcrsReturn :: Lens' DeleteVPCPeeringConnectionResponse (Maybe Bool)
dvpcrsReturn = lens _dvpcrsReturn (\ s a -> s{_dvpcrsReturn = a})

-- | -- | The response status code.
dvpcrsResponseStatus :: Lens' DeleteVPCPeeringConnectionResponse Int
dvpcrsResponseStatus = lens _dvpcrsResponseStatus (\ s a -> s{_dvpcrsResponseStatus = a})

instance NFData DeleteVPCPeeringConnectionResponse
         where
