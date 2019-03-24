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
-- Module      : Network.AWS.EC2.CreateTransitGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit gateway.
--
--
-- You can use a transit gateway to interconnect your virtual private clouds (VPC) and on-premises networks. After the transit gateway enters the @available@ state, you can attach your VPCs and VPN connections to the transit gateway.
--
-- To attach your VPCs, use 'CreateTransitGatewayVpcAttachment' .
--
-- To attach a VPN connection, use 'CreateCustomerGateway' to create a customer gateway and specify the ID of the customer gateway and the ID of the transit gateway in a call to 'CreateVpnConnection' .
--
-- When you create a transit gateway, we create a default transit gateway route table and use it as the default association route table and the default propagation route table. You can use 'CreateTransitGatewayRouteTable' to create additional transit gateway route tables. If you disable automatic route propagation, we do not create a default transit gateway route table. You can use 'EnableTransitGatewayRouteTablePropagation' to propagate routes from a resource attachment to a transit gateway route table. If you disable automatic associations, you can use 'AssociateTransitGatewayRouteTable' to associate a resource attachment with a transit gateway route table.
--
module Network.AWS.EC2.CreateTransitGateway
    (
    -- * Creating a Request
      createTransitGateway
    , CreateTransitGateway
    -- * Request Lenses
    , ctgTagSpecifications
    , ctgOptions
    , ctgDescription
    , ctgDryRun

    -- * Destructuring the Response
    , createTransitGatewayResponse
    , CreateTransitGatewayResponse
    -- * Response Lenses
    , ctgrsTransitGateway
    , ctgrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTransitGateway' smart constructor.
data CreateTransitGateway = CreateTransitGateway'
  { _ctgTagSpecifications :: !(Maybe [TagSpecification])
  , _ctgOptions           :: !(Maybe TransitGatewayRequestOptions)
  , _ctgDescription       :: !(Maybe Text)
  , _ctgDryRun            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgTagSpecifications' - The tags to apply to the transit gateway.
--
-- * 'ctgOptions' - The transit gateway options.
--
-- * 'ctgDescription' - A description of the transit gateway.
--
-- * 'ctgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
createTransitGateway
    :: CreateTransitGateway
createTransitGateway =
  CreateTransitGateway'
    { _ctgTagSpecifications = Nothing
    , _ctgOptions = Nothing
    , _ctgDescription = Nothing
    , _ctgDryRun = Nothing
    }


-- | The tags to apply to the transit gateway.
ctgTagSpecifications :: Lens' CreateTransitGateway [TagSpecification]
ctgTagSpecifications = lens _ctgTagSpecifications (\ s a -> s{_ctgTagSpecifications = a}) . _Default . _Coerce

-- | The transit gateway options.
ctgOptions :: Lens' CreateTransitGateway (Maybe TransitGatewayRequestOptions)
ctgOptions = lens _ctgOptions (\ s a -> s{_ctgOptions = a})

-- | A description of the transit gateway.
ctgDescription :: Lens' CreateTransitGateway (Maybe Text)
ctgDescription = lens _ctgDescription (\ s a -> s{_ctgDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ctgDryRun :: Lens' CreateTransitGateway (Maybe Bool)
ctgDryRun = lens _ctgDryRun (\ s a -> s{_ctgDryRun = a})

instance AWSRequest CreateTransitGateway where
        type Rs CreateTransitGateway =
             CreateTransitGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateTransitGatewayResponse' <$>
                   (x .@? "transitGateway") <*> (pure (fromEnum s)))

instance Hashable CreateTransitGateway where

instance NFData CreateTransitGateway where

instance ToHeaders CreateTransitGateway where
        toHeaders = const mempty

instance ToPath CreateTransitGateway where
        toPath = const "/"

instance ToQuery CreateTransitGateway where
        toQuery CreateTransitGateway'{..}
          = mconcat
              ["Action" =: ("CreateTransitGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _ctgTagSpecifications),
               "Options" =: _ctgOptions,
               "Description" =: _ctgDescription,
               "DryRun" =: _ctgDryRun]

-- | /See:/ 'createTransitGatewayResponse' smart constructor.
data CreateTransitGatewayResponse = CreateTransitGatewayResponse'
  { _ctgrsTransitGateway :: !(Maybe TransitGateway)
  , _ctgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTransitGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrsTransitGateway' - Information about the transit gateway.
--
-- * 'ctgrsResponseStatus' - -- | The response status code.
createTransitGatewayResponse
    :: Int -- ^ 'ctgrsResponseStatus'
    -> CreateTransitGatewayResponse
createTransitGatewayResponse pResponseStatus_ =
  CreateTransitGatewayResponse'
    {_ctgrsTransitGateway = Nothing, _ctgrsResponseStatus = pResponseStatus_}


-- | Information about the transit gateway.
ctgrsTransitGateway :: Lens' CreateTransitGatewayResponse (Maybe TransitGateway)
ctgrsTransitGateway = lens _ctgrsTransitGateway (\ s a -> s{_ctgrsTransitGateway = a})

-- | -- | The response status code.
ctgrsResponseStatus :: Lens' CreateTransitGatewayResponse Int
ctgrsResponseStatus = lens _ctgrsResponseStatus (\ s a -> s{_ctgrsResponseStatus = a})

instance NFData CreateTransitGatewayResponse where
