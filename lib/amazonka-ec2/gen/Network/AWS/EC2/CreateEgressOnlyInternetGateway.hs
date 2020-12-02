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
-- Module      : Network.AWS.EC2.CreateEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [IPv6 only] Creates an egress-only Internet gateway for your VPC. An egress-only Internet gateway is used to enable outbound communication over IPv6 from instances in your VPC to the Internet, and prevents hosts outside of your VPC from initiating an IPv6 connection with your instance.
--
--
module Network.AWS.EC2.CreateEgressOnlyInternetGateway
    (
    -- * Creating a Request
      createEgressOnlyInternetGateway
    , CreateEgressOnlyInternetGateway
    -- * Request Lenses
    , ceoigClientToken
    , ceoigDryRun
    , ceoigVPCId

    -- * Destructuring the Response
    , createEgressOnlyInternetGatewayResponse
    , CreateEgressOnlyInternetGatewayResponse
    -- * Response Lenses
    , ceoigrsClientToken
    , ceoigrsEgressOnlyInternetGateway
    , ceoigrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEgressOnlyInternetGateway' smart constructor.
data CreateEgressOnlyInternetGateway = CreateEgressOnlyInternetGateway'
  { _ceoigClientToken :: !(Maybe Text)
  , _ceoigDryRun      :: !(Maybe Bool)
  , _ceoigVPCId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceoigClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ceoigDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ceoigVPCId' - The ID of the VPC for which to create the egress-only Internet gateway.
createEgressOnlyInternetGateway
    :: Text -- ^ 'ceoigVPCId'
    -> CreateEgressOnlyInternetGateway
createEgressOnlyInternetGateway pVPCId_ =
  CreateEgressOnlyInternetGateway'
    {_ceoigClientToken = Nothing, _ceoigDryRun = Nothing, _ceoigVPCId = pVPCId_}


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ceoigClientToken :: Lens' CreateEgressOnlyInternetGateway (Maybe Text)
ceoigClientToken = lens _ceoigClientToken (\ s a -> s{_ceoigClientToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ceoigDryRun :: Lens' CreateEgressOnlyInternetGateway (Maybe Bool)
ceoigDryRun = lens _ceoigDryRun (\ s a -> s{_ceoigDryRun = a})

-- | The ID of the VPC for which to create the egress-only Internet gateway.
ceoigVPCId :: Lens' CreateEgressOnlyInternetGateway Text
ceoigVPCId = lens _ceoigVPCId (\ s a -> s{_ceoigVPCId = a})

instance AWSRequest CreateEgressOnlyInternetGateway
         where
        type Rs CreateEgressOnlyInternetGateway =
             CreateEgressOnlyInternetGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateEgressOnlyInternetGatewayResponse' <$>
                   (x .@? "clientToken") <*>
                     (x .@? "egressOnlyInternetGateway")
                     <*> (pure (fromEnum s)))

instance Hashable CreateEgressOnlyInternetGateway
         where

instance NFData CreateEgressOnlyInternetGateway where

instance ToHeaders CreateEgressOnlyInternetGateway
         where
        toHeaders = const mempty

instance ToPath CreateEgressOnlyInternetGateway where
        toPath = const "/"

instance ToQuery CreateEgressOnlyInternetGateway
         where
        toQuery CreateEgressOnlyInternetGateway'{..}
          = mconcat
              ["Action" =:
                 ("CreateEgressOnlyInternetGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _ceoigClientToken,
               "DryRun" =: _ceoigDryRun, "VpcId" =: _ceoigVPCId]

-- | /See:/ 'createEgressOnlyInternetGatewayResponse' smart constructor.
data CreateEgressOnlyInternetGatewayResponse = CreateEgressOnlyInternetGatewayResponse'
  { _ceoigrsClientToken               :: !(Maybe Text)
  , _ceoigrsEgressOnlyInternetGateway :: !(Maybe EgressOnlyInternetGateway)
  , _ceoigrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEgressOnlyInternetGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceoigrsClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
--
-- * 'ceoigrsEgressOnlyInternetGateway' - Information about the egress-only Internet gateway.
--
-- * 'ceoigrsResponseStatus' - -- | The response status code.
createEgressOnlyInternetGatewayResponse
    :: Int -- ^ 'ceoigrsResponseStatus'
    -> CreateEgressOnlyInternetGatewayResponse
createEgressOnlyInternetGatewayResponse pResponseStatus_ =
  CreateEgressOnlyInternetGatewayResponse'
    { _ceoigrsClientToken = Nothing
    , _ceoigrsEgressOnlyInternetGateway = Nothing
    , _ceoigrsResponseStatus = pResponseStatus_
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
ceoigrsClientToken :: Lens' CreateEgressOnlyInternetGatewayResponse (Maybe Text)
ceoigrsClientToken = lens _ceoigrsClientToken (\ s a -> s{_ceoigrsClientToken = a})

-- | Information about the egress-only Internet gateway.
ceoigrsEgressOnlyInternetGateway :: Lens' CreateEgressOnlyInternetGatewayResponse (Maybe EgressOnlyInternetGateway)
ceoigrsEgressOnlyInternetGateway = lens _ceoigrsEgressOnlyInternetGateway (\ s a -> s{_ceoigrsEgressOnlyInternetGateway = a})

-- | -- | The response status code.
ceoigrsResponseStatus :: Lens' CreateEgressOnlyInternetGatewayResponse Int
ceoigrsResponseStatus = lens _ceoigrsResponseStatus (\ s a -> s{_ceoigrsResponseStatus = a})

instance NFData
           CreateEgressOnlyInternetGatewayResponse
         where
