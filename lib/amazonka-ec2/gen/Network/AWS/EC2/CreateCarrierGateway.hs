{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateCarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a carrier gateway. For more information about carrier gateways, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#wavelength-carrier-gateway Carrier gateways> in the /AWS Wavelength Developer Guide/ .
module Network.AWS.EC2.CreateCarrierGateway
  ( -- * Creating a Request
    createCarrierGateway,
    CreateCarrierGateway,

    -- * Request Lenses
    ccgcClientToken,
    ccgcTagSpecifications,
    ccgcDryRun,
    ccgcVPCId,

    -- * Destructuring the Response
    createCarrierGatewayResponse,
    CreateCarrierGatewayResponse,

    -- * Response Lenses
    ccgcrsCarrierGateway,
    ccgcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCarrierGateway' smart constructor.
data CreateCarrierGateway = CreateCarrierGateway'
  { _ccgcClientToken ::
      !(Maybe Text),
    _ccgcTagSpecifications ::
      !(Maybe [TagSpecification]),
    _ccgcDryRun :: !(Maybe Bool),
    _ccgcVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCarrierGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccgcClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'ccgcTagSpecifications' - The tags to associate with the carrier gateway.
--
-- * 'ccgcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ccgcVPCId' - The ID of the VPC to associate with the carrier gateway.
createCarrierGateway ::
  -- | 'ccgcVPCId'
  Text ->
  CreateCarrierGateway
createCarrierGateway pVPCId_ =
  CreateCarrierGateway'
    { _ccgcClientToken = Nothing,
      _ccgcTagSpecifications = Nothing,
      _ccgcDryRun = Nothing,
      _ccgcVPCId = pVPCId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
ccgcClientToken :: Lens' CreateCarrierGateway (Maybe Text)
ccgcClientToken = lens _ccgcClientToken (\s a -> s {_ccgcClientToken = a})

-- | The tags to associate with the carrier gateway.
ccgcTagSpecifications :: Lens' CreateCarrierGateway [TagSpecification]
ccgcTagSpecifications = lens _ccgcTagSpecifications (\s a -> s {_ccgcTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ccgcDryRun :: Lens' CreateCarrierGateway (Maybe Bool)
ccgcDryRun = lens _ccgcDryRun (\s a -> s {_ccgcDryRun = a})

-- | The ID of the VPC to associate with the carrier gateway.
ccgcVPCId :: Lens' CreateCarrierGateway Text
ccgcVPCId = lens _ccgcVPCId (\s a -> s {_ccgcVPCId = a})

instance AWSRequest CreateCarrierGateway where
  type Rs CreateCarrierGateway = CreateCarrierGatewayResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateCarrierGatewayResponse'
            <$> (x .@? "carrierGateway") <*> (pure (fromEnum s))
      )

instance Hashable CreateCarrierGateway

instance NFData CreateCarrierGateway

instance ToHeaders CreateCarrierGateway where
  toHeaders = const mempty

instance ToPath CreateCarrierGateway where
  toPath = const "/"

instance ToQuery CreateCarrierGateway where
  toQuery CreateCarrierGateway' {..} =
    mconcat
      [ "Action" =: ("CreateCarrierGateway" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _ccgcClientToken,
        toQuery
          (toQueryList "TagSpecification" <$> _ccgcTagSpecifications),
        "DryRun" =: _ccgcDryRun,
        "VpcId" =: _ccgcVPCId
      ]

-- | /See:/ 'createCarrierGatewayResponse' smart constructor.
data CreateCarrierGatewayResponse = CreateCarrierGatewayResponse'
  { _ccgcrsCarrierGateway ::
      !(Maybe CarrierGateway),
    _ccgcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCarrierGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccgcrsCarrierGateway' - Information about the carrier gateway.
--
-- * 'ccgcrsResponseStatus' - -- | The response status code.
createCarrierGatewayResponse ::
  -- | 'ccgcrsResponseStatus'
  Int ->
  CreateCarrierGatewayResponse
createCarrierGatewayResponse pResponseStatus_ =
  CreateCarrierGatewayResponse'
    { _ccgcrsCarrierGateway = Nothing,
      _ccgcrsResponseStatus = pResponseStatus_
    }

-- | Information about the carrier gateway.
ccgcrsCarrierGateway :: Lens' CreateCarrierGatewayResponse (Maybe CarrierGateway)
ccgcrsCarrierGateway = lens _ccgcrsCarrierGateway (\s a -> s {_ccgcrsCarrierGateway = a})

-- | -- | The response status code.
ccgcrsResponseStatus :: Lens' CreateCarrierGatewayResponse Int
ccgcrsResponseStatus = lens _ccgcrsResponseStatus (\s a -> s {_ccgcrsResponseStatus = a})

instance NFData CreateCarrierGatewayResponse
