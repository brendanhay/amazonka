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
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Direct Connect gateway, which is an intermediate object that enables you to connect a set of virtual interfaces and virtual private gateways. A Direct Connect gateway is global and visible in any AWS Region after it is created. The virtual interfaces and virtual private gateways that are connected through a Direct Connect gateway can be in different AWS Regions. This enables you to connect to a VPC in any Region, regardless of the Region in which the virtual interfaces are located, and pass traffic between them.
module Network.AWS.DirectConnect.CreateDirectConnectGateway
  ( -- * Creating a Request
    createDirectConnectGateway,
    CreateDirectConnectGateway,

    -- * Request Lenses
    cdcgAmazonSideASN,
    cdcgDirectConnectGatewayName,

    -- * Destructuring the Response
    createDirectConnectGatewayResponse,
    CreateDirectConnectGatewayResponse,

    -- * Response Lenses
    cdcgrsDirectConnectGateway,
    cdcgrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDirectConnectGateway' smart constructor.
data CreateDirectConnectGateway = CreateDirectConnectGateway'
  { _cdcgAmazonSideASN ::
      !(Maybe Integer),
    _cdcgDirectConnectGatewayName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDirectConnectGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcgAmazonSideASN' - The autonomous system number (ASN) for Border Gateway Protocol (BGP) to be configured on the Amazon side of the connection. The ASN must be in the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294. The default is 64512.
--
-- * 'cdcgDirectConnectGatewayName' - The name of the Direct Connect gateway.
createDirectConnectGateway ::
  -- | 'cdcgDirectConnectGatewayName'
  Text ->
  CreateDirectConnectGateway
createDirectConnectGateway pDirectConnectGatewayName_ =
  CreateDirectConnectGateway'
    { _cdcgAmazonSideASN = Nothing,
      _cdcgDirectConnectGatewayName = pDirectConnectGatewayName_
    }

-- | The autonomous system number (ASN) for Border Gateway Protocol (BGP) to be configured on the Amazon side of the connection. The ASN must be in the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294. The default is 64512.
cdcgAmazonSideASN :: Lens' CreateDirectConnectGateway (Maybe Integer)
cdcgAmazonSideASN = lens _cdcgAmazonSideASN (\s a -> s {_cdcgAmazonSideASN = a})

-- | The name of the Direct Connect gateway.
cdcgDirectConnectGatewayName :: Lens' CreateDirectConnectGateway Text
cdcgDirectConnectGatewayName = lens _cdcgDirectConnectGatewayName (\s a -> s {_cdcgDirectConnectGatewayName = a})

instance AWSRequest CreateDirectConnectGateway where
  type
    Rs CreateDirectConnectGateway =
      CreateDirectConnectGatewayResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayResponse'
            <$> (x .?> "directConnectGateway") <*> (pure (fromEnum s))
      )

instance Hashable CreateDirectConnectGateway

instance NFData CreateDirectConnectGateway

instance ToHeaders CreateDirectConnectGateway where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.CreateDirectConnectGateway" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDirectConnectGateway where
  toJSON CreateDirectConnectGateway' {..} =
    object
      ( catMaybes
          [ ("amazonSideAsn" .=) <$> _cdcgAmazonSideASN,
            Just
              ("directConnectGatewayName" .= _cdcgDirectConnectGatewayName)
          ]
      )

instance ToPath CreateDirectConnectGateway where
  toPath = const "/"

instance ToQuery CreateDirectConnectGateway where
  toQuery = const mempty

-- | /See:/ 'createDirectConnectGatewayResponse' smart constructor.
data CreateDirectConnectGatewayResponse = CreateDirectConnectGatewayResponse'
  { _cdcgrsDirectConnectGateway ::
      !( Maybe
           DirectConnectGateway
       ),
    _cdcgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDirectConnectGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcgrsDirectConnectGateway' - The Direct Connect gateway.
--
-- * 'cdcgrsResponseStatus' - -- | The response status code.
createDirectConnectGatewayResponse ::
  -- | 'cdcgrsResponseStatus'
  Int ->
  CreateDirectConnectGatewayResponse
createDirectConnectGatewayResponse pResponseStatus_ =
  CreateDirectConnectGatewayResponse'
    { _cdcgrsDirectConnectGateway =
        Nothing,
      _cdcgrsResponseStatus = pResponseStatus_
    }

-- | The Direct Connect gateway.
cdcgrsDirectConnectGateway :: Lens' CreateDirectConnectGatewayResponse (Maybe DirectConnectGateway)
cdcgrsDirectConnectGateway = lens _cdcgrsDirectConnectGateway (\s a -> s {_cdcgrsDirectConnectGateway = a})

-- | -- | The response status code.
cdcgrsResponseStatus :: Lens' CreateDirectConnectGatewayResponse Int
cdcgrsResponseStatus = lens _cdcgrsResponseStatus (\s a -> s {_cdcgrsResponseStatus = a})

instance NFData CreateDirectConnectGatewayResponse
