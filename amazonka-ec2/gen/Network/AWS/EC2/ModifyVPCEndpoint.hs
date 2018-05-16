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
-- Module      : Network.AWS.EC2.ModifyVPCEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes of a specified VPC endpoint. The attributes that you can modify depend on the type of VPC endpoint (interface or gateway). For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-endpoints.html VPC Endpoints> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
module Network.AWS.EC2.ModifyVPCEndpoint
    (
    -- * Creating a Request
      modifyVPCEndpoint
    , ModifyVPCEndpoint
    -- * Request Lenses
    , mvePolicyDocument
    , mveRemoveRouteTableIds
    , mveResetPolicy
    , mveAddRouteTableIds
    , mvePrivateDNSEnabled
    , mveAddSubnetIds
    , mveRemoveSubnetIds
    , mveAddSecurityGroupIds
    , mveDryRun
    , mveRemoveSecurityGroupIds
    , mveVPCEndpointId

    -- * Destructuring the Response
    , modifyVPCEndpointResponse
    , ModifyVPCEndpointResponse
    -- * Response Lenses
    , mversReturn
    , mversResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyVpcEndpoint.
--
--
--
-- /See:/ 'modifyVPCEndpoint' smart constructor.
data ModifyVPCEndpoint = ModifyVPCEndpoint'
  { _mvePolicyDocument         :: !(Maybe Text)
  , _mveRemoveRouteTableIds    :: !(Maybe [Text])
  , _mveResetPolicy            :: !(Maybe Bool)
  , _mveAddRouteTableIds       :: !(Maybe [Text])
  , _mvePrivateDNSEnabled      :: !(Maybe Bool)
  , _mveAddSubnetIds           :: !(Maybe [Text])
  , _mveRemoveSubnetIds        :: !(Maybe [Text])
  , _mveAddSecurityGroupIds    :: !(Maybe [Text])
  , _mveDryRun                 :: !(Maybe Bool)
  , _mveRemoveSecurityGroupIds :: !(Maybe [Text])
  , _mveVPCEndpointId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvePolicyDocument' - (Gateway endpoint) A policy document to attach to the endpoint. The policy must be in valid JSON format.
--
-- * 'mveRemoveRouteTableIds' - (Gateway endpoint) One or more route table IDs to disassociate from the endpoint.
--
-- * 'mveResetPolicy' - (Gateway endpoint) Specify @true@ to reset the policy document to the default policy. The default policy allows full access to the service.
--
-- * 'mveAddRouteTableIds' - (Gateway endpoint) One or more route tables IDs to associate with the endpoint.
--
-- * 'mvePrivateDNSEnabled' - (Interface endpoint) Indicate whether a private hosted zone is associated with the VPC.
--
-- * 'mveAddSubnetIds' - (Interface endpoint) One or more subnet IDs in which to serve the endpoint.
--
-- * 'mveRemoveSubnetIds' - (Interface endpoint) One or more subnets IDs in which to remove the endpoint.
--
-- * 'mveAddSecurityGroupIds' - (Interface endpoint) One or more security group IDs to associate with the network interface.
--
-- * 'mveDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mveRemoveSecurityGroupIds' - (Interface endpoint) One or more security group IDs to disassociate from the network interface.
--
-- * 'mveVPCEndpointId' - The ID of the endpoint.
modifyVPCEndpoint
    :: Text -- ^ 'mveVPCEndpointId'
    -> ModifyVPCEndpoint
modifyVPCEndpoint pVPCEndpointId_ =
  ModifyVPCEndpoint'
    { _mvePolicyDocument = Nothing
    , _mveRemoveRouteTableIds = Nothing
    , _mveResetPolicy = Nothing
    , _mveAddRouteTableIds = Nothing
    , _mvePrivateDNSEnabled = Nothing
    , _mveAddSubnetIds = Nothing
    , _mveRemoveSubnetIds = Nothing
    , _mveAddSecurityGroupIds = Nothing
    , _mveDryRun = Nothing
    , _mveRemoveSecurityGroupIds = Nothing
    , _mveVPCEndpointId = pVPCEndpointId_
    }


-- | (Gateway endpoint) A policy document to attach to the endpoint. The policy must be in valid JSON format.
mvePolicyDocument :: Lens' ModifyVPCEndpoint (Maybe Text)
mvePolicyDocument = lens _mvePolicyDocument (\ s a -> s{_mvePolicyDocument = a})

-- | (Gateway endpoint) One or more route table IDs to disassociate from the endpoint.
mveRemoveRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mveRemoveRouteTableIds = lens _mveRemoveRouteTableIds (\ s a -> s{_mveRemoveRouteTableIds = a}) . _Default . _Coerce

-- | (Gateway endpoint) Specify @true@ to reset the policy document to the default policy. The default policy allows full access to the service.
mveResetPolicy :: Lens' ModifyVPCEndpoint (Maybe Bool)
mveResetPolicy = lens _mveResetPolicy (\ s a -> s{_mveResetPolicy = a})

-- | (Gateway endpoint) One or more route tables IDs to associate with the endpoint.
mveAddRouteTableIds :: Lens' ModifyVPCEndpoint [Text]
mveAddRouteTableIds = lens _mveAddRouteTableIds (\ s a -> s{_mveAddRouteTableIds = a}) . _Default . _Coerce

-- | (Interface endpoint) Indicate whether a private hosted zone is associated with the VPC.
mvePrivateDNSEnabled :: Lens' ModifyVPCEndpoint (Maybe Bool)
mvePrivateDNSEnabled = lens _mvePrivateDNSEnabled (\ s a -> s{_mvePrivateDNSEnabled = a})

-- | (Interface endpoint) One or more subnet IDs in which to serve the endpoint.
mveAddSubnetIds :: Lens' ModifyVPCEndpoint [Text]
mveAddSubnetIds = lens _mveAddSubnetIds (\ s a -> s{_mveAddSubnetIds = a}) . _Default . _Coerce

-- | (Interface endpoint) One or more subnets IDs in which to remove the endpoint.
mveRemoveSubnetIds :: Lens' ModifyVPCEndpoint [Text]
mveRemoveSubnetIds = lens _mveRemoveSubnetIds (\ s a -> s{_mveRemoveSubnetIds = a}) . _Default . _Coerce

-- | (Interface endpoint) One or more security group IDs to associate with the network interface.
mveAddSecurityGroupIds :: Lens' ModifyVPCEndpoint [Text]
mveAddSecurityGroupIds = lens _mveAddSecurityGroupIds (\ s a -> s{_mveAddSecurityGroupIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mveDryRun :: Lens' ModifyVPCEndpoint (Maybe Bool)
mveDryRun = lens _mveDryRun (\ s a -> s{_mveDryRun = a})

-- | (Interface endpoint) One or more security group IDs to disassociate from the network interface.
mveRemoveSecurityGroupIds :: Lens' ModifyVPCEndpoint [Text]
mveRemoveSecurityGroupIds = lens _mveRemoveSecurityGroupIds (\ s a -> s{_mveRemoveSecurityGroupIds = a}) . _Default . _Coerce

-- | The ID of the endpoint.
mveVPCEndpointId :: Lens' ModifyVPCEndpoint Text
mveVPCEndpointId = lens _mveVPCEndpointId (\ s a -> s{_mveVPCEndpointId = a})

instance AWSRequest ModifyVPCEndpoint where
        type Rs ModifyVPCEndpoint = ModifyVPCEndpointResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVPCEndpointResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifyVPCEndpoint where

instance NFData ModifyVPCEndpoint where

instance ToHeaders ModifyVPCEndpoint where
        toHeaders = const mempty

instance ToPath ModifyVPCEndpoint where
        toPath = const "/"

instance ToQuery ModifyVPCEndpoint where
        toQuery ModifyVPCEndpoint'{..}
          = mconcat
              ["Action" =: ("ModifyVpcEndpoint" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "PolicyDocument" =: _mvePolicyDocument,
               toQuery
                 (toQueryList "RemoveRouteTableId" <$>
                    _mveRemoveRouteTableIds),
               "ResetPolicy" =: _mveResetPolicy,
               toQuery
                 (toQueryList "AddRouteTableId" <$>
                    _mveAddRouteTableIds),
               "PrivateDnsEnabled" =: _mvePrivateDNSEnabled,
               toQuery
                 (toQueryList "AddSubnetId" <$> _mveAddSubnetIds),
               toQuery
                 (toQueryList "RemoveSubnetId" <$>
                    _mveRemoveSubnetIds),
               toQuery
                 (toQueryList "AddSecurityGroupId" <$>
                    _mveAddSecurityGroupIds),
               "DryRun" =: _mveDryRun,
               toQuery
                 (toQueryList "RemoveSecurityGroupId" <$>
                    _mveRemoveSecurityGroupIds),
               "VpcEndpointId" =: _mveVPCEndpointId]

-- | /See:/ 'modifyVPCEndpointResponse' smart constructor.
data ModifyVPCEndpointResponse = ModifyVPCEndpointResponse'
  { _mversReturn         :: !(Maybe Bool)
  , _mversResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mversReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'mversResponseStatus' - -- | The response status code.
modifyVPCEndpointResponse
    :: Int -- ^ 'mversResponseStatus'
    -> ModifyVPCEndpointResponse
modifyVPCEndpointResponse pResponseStatus_ =
  ModifyVPCEndpointResponse'
    {_mversReturn = Nothing, _mversResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
mversReturn :: Lens' ModifyVPCEndpointResponse (Maybe Bool)
mversReturn = lens _mversReturn (\ s a -> s{_mversReturn = a})

-- | -- | The response status code.
mversResponseStatus :: Lens' ModifyVPCEndpointResponse Int
mversResponseStatus = lens _mversResponseStatus (\ s a -> s{_mversResponseStatus = a})

instance NFData ModifyVPCEndpointResponse where
