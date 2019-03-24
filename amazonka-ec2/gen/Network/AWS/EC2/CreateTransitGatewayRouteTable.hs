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
-- Module      : Network.AWS.EC2.CreateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified transit gateway.
--
--
module Network.AWS.EC2.CreateTransitGatewayRouteTable
    (
    -- * Creating a Request
      createTransitGatewayRouteTable
    , CreateTransitGatewayRouteTable
    -- * Request Lenses
    , ctgrtTagSpecifications
    , ctgrtDryRun
    , ctgrtTransitGatewayId

    -- * Destructuring the Response
    , createTransitGatewayRouteTableResponse
    , CreateTransitGatewayRouteTableResponse
    -- * Response Lenses
    , ctgrtrsTransitGatewayRouteTable
    , ctgrtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitGatewayRouteTable' smart constructor.
data CreateTransitGatewayRouteTable = CreateTransitGatewayRouteTable'
  { _ctgrtTagSpecifications :: !(Maybe [TagSpecification])
  , _ctgrtDryRun            :: !(Maybe Bool)
  , _ctgrtTransitGatewayId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrtTagSpecifications' - The tags to apply to the transit gateway route table.
--
-- * 'ctgrtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ctgrtTransitGatewayId' - The ID of the transit gateway.
createTransitGatewayRouteTable
    :: Text -- ^ 'ctgrtTransitGatewayId'
    -> CreateTransitGatewayRouteTable
createTransitGatewayRouteTable pTransitGatewayId_ =
  CreateTransitGatewayRouteTable'
    { _ctgrtTagSpecifications = Nothing
    , _ctgrtDryRun = Nothing
    , _ctgrtTransitGatewayId = pTransitGatewayId_
    }


-- | The tags to apply to the transit gateway route table.
ctgrtTagSpecifications :: Lens' CreateTransitGatewayRouteTable [TagSpecification]
ctgrtTagSpecifications = lens _ctgrtTagSpecifications (\ s a -> s{_ctgrtTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctgrtDryRun :: Lens' CreateTransitGatewayRouteTable (Maybe Bool)
ctgrtDryRun = lens _ctgrtDryRun (\ s a -> s{_ctgrtDryRun = a})

-- | The ID of the transit gateway.
ctgrtTransitGatewayId :: Lens' CreateTransitGatewayRouteTable Text
ctgrtTransitGatewayId = lens _ctgrtTransitGatewayId (\ s a -> s{_ctgrtTransitGatewayId = a})

instance AWSRequest CreateTransitGatewayRouteTable
         where
        type Rs CreateTransitGatewayRouteTable =
             CreateTransitGatewayRouteTableResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateTransitGatewayRouteTableResponse' <$>
                   (x .@? "transitGatewayRouteTable") <*>
                     (pure (fromEnum s)))

instance Hashable CreateTransitGatewayRouteTable
         where

instance NFData CreateTransitGatewayRouteTable where

instance ToHeaders CreateTransitGatewayRouteTable
         where
        toHeaders = const mempty

instance ToPath CreateTransitGatewayRouteTable where
        toPath = const "/"

instance ToQuery CreateTransitGatewayRouteTable where
        toQuery CreateTransitGatewayRouteTable'{..}
          = mconcat
              ["Action" =:
                 ("CreateTransitGatewayRouteTable" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TagSpecifications" <$>
                    _ctgrtTagSpecifications),
               "DryRun" =: _ctgrtDryRun,
               "TransitGatewayId" =: _ctgrtTransitGatewayId]

-- | /See:/ 'createTransitGatewayRouteTableResponse' smart constructor.
data CreateTransitGatewayRouteTableResponse = CreateTransitGatewayRouteTableResponse'
  { _ctgrtrsTransitGatewayRouteTable :: !(Maybe TransitGatewayRouteTable)
  , _ctgrtrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrtrsTransitGatewayRouteTable' - Information about the transit gateway route table.
--
-- * 'ctgrtrsResponseStatus' - -- | The response status code.
createTransitGatewayRouteTableResponse
    :: Int -- ^ 'ctgrtrsResponseStatus'
    -> CreateTransitGatewayRouteTableResponse
createTransitGatewayRouteTableResponse pResponseStatus_ =
  CreateTransitGatewayRouteTableResponse'
    { _ctgrtrsTransitGatewayRouteTable = Nothing
    , _ctgrtrsResponseStatus = pResponseStatus_
    }


-- | Information about the transit gateway route table.
ctgrtrsTransitGatewayRouteTable :: Lens' CreateTransitGatewayRouteTableResponse (Maybe TransitGatewayRouteTable)
ctgrtrsTransitGatewayRouteTable = lens _ctgrtrsTransitGatewayRouteTable (\ s a -> s{_ctgrtrsTransitGatewayRouteTable = a})

-- | -- | The response status code.
ctgrtrsResponseStatus :: Lens' CreateTransitGatewayRouteTableResponse Int
ctgrtrsResponseStatus = lens _ctgrtrsResponseStatus (\ s a -> s{_ctgrtrsResponseStatus = a})

instance NFData
           CreateTransitGatewayRouteTableResponse
         where
