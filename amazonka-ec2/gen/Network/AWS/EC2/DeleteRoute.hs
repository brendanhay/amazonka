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
-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified route table.
--
--
module Network.AWS.EC2.DeleteRoute
    (
    -- * Creating a Request
      deleteRoute
    , DeleteRoute
    -- * Request Lenses
    , drDestinationIPv6CidrBlock
    , drDryRun
    , drDestinationCidrBlock
    , drRouteTableId

    -- * Destructuring the Response
    , deleteRouteResponse
    , DeleteRouteResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { _drDestinationIPv6CidrBlock :: !(Maybe Text)
  , _drDryRun                   :: !(Maybe Bool)
  , _drDestinationCidrBlock     :: !(Maybe Text)
  , _drRouteTableId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drDestinationIPv6CidrBlock' - The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- * 'drDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'drDestinationCidrBlock' - The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- * 'drRouteTableId' - The ID of the route table.
deleteRoute
    :: Text -- ^ 'drRouteTableId'
    -> DeleteRoute
deleteRoute pRouteTableId_ =
  DeleteRoute'
    { _drDestinationIPv6CidrBlock = Nothing
    , _drDryRun = Nothing
    , _drDestinationCidrBlock = Nothing
    , _drRouteTableId = pRouteTableId_
    }


-- | The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
drDestinationIPv6CidrBlock :: Lens' DeleteRoute (Maybe Text)
drDestinationIPv6CidrBlock = lens _drDestinationIPv6CidrBlock (\ s a -> s{_drDestinationIPv6CidrBlock = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
drDryRun :: Lens' DeleteRoute (Maybe Bool)
drDryRun = lens _drDryRun (\ s a -> s{_drDryRun = a})

-- | The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
drDestinationCidrBlock :: Lens' DeleteRoute (Maybe Text)
drDestinationCidrBlock = lens _drDestinationCidrBlock (\ s a -> s{_drDestinationCidrBlock = a})

-- | The ID of the route table.
drRouteTableId :: Lens' DeleteRoute Text
drRouteTableId = lens _drRouteTableId (\ s a -> s{_drRouteTableId = a})

instance AWSRequest DeleteRoute where
        type Rs DeleteRoute = DeleteRouteResponse
        request = postQuery ec2
        response = receiveNull DeleteRouteResponse'

instance Hashable DeleteRoute where

instance NFData DeleteRoute where

instance ToHeaders DeleteRoute where
        toHeaders = const mempty

instance ToPath DeleteRoute where
        toPath = const "/"

instance ToQuery DeleteRoute where
        toQuery DeleteRoute'{..}
          = mconcat
              ["Action" =: ("DeleteRoute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DestinationIpv6CidrBlock" =:
                 _drDestinationIPv6CidrBlock,
               "DryRun" =: _drDryRun,
               "DestinationCidrBlock" =: _drDestinationCidrBlock,
               "RouteTableId" =: _drRouteTableId]

-- | /See:/ 'deleteRouteResponse' smart constructor.
data DeleteRouteResponse =
  DeleteRouteResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRouteResponse' with the minimum fields required to make a request.
--
deleteRouteResponse
    :: DeleteRouteResponse
deleteRouteResponse = DeleteRouteResponse'


instance NFData DeleteRouteResponse where
