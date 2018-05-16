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
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a direct connect gateway and a virtual private gateway (VGW). The VGW must be attached to a VPC and must not be associated with another direct connect gateway.
--
--
module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
    (
    -- * Creating a Request
      createDirectConnectGatewayAssociation
    , CreateDirectConnectGatewayAssociation
    -- * Request Lenses
    , cdcgaDirectConnectGatewayId
    , cdcgaVirtualGatewayId

    -- * Destructuring the Response
    , createDirectConnectGatewayAssociationResponse
    , CreateDirectConnectGatewayAssociationResponse
    -- * Response Lenses
    , cdcgarsDirectConnectGatewayAssociation
    , cdcgarsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the CreateDirectConnectGatewayAssociation operation.
--
--
--
-- /See:/ 'createDirectConnectGatewayAssociation' smart constructor.
data CreateDirectConnectGatewayAssociation = CreateDirectConnectGatewayAssociation'
  { _cdcgaDirectConnectGatewayId :: !Text
  , _cdcgaVirtualGatewayId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcgaDirectConnectGatewayId' - The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
--
-- * 'cdcgaVirtualGatewayId' - The ID of the virtual private gateway. Example: "vgw-abc123ef" Default: None
createDirectConnectGatewayAssociation
    :: Text -- ^ 'cdcgaDirectConnectGatewayId'
    -> Text -- ^ 'cdcgaVirtualGatewayId'
    -> CreateDirectConnectGatewayAssociation
createDirectConnectGatewayAssociation pDirectConnectGatewayId_ pVirtualGatewayId_ =
  CreateDirectConnectGatewayAssociation'
    { _cdcgaDirectConnectGatewayId = pDirectConnectGatewayId_
    , _cdcgaVirtualGatewayId = pVirtualGatewayId_
    }


-- | The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
cdcgaDirectConnectGatewayId :: Lens' CreateDirectConnectGatewayAssociation Text
cdcgaDirectConnectGatewayId = lens _cdcgaDirectConnectGatewayId (\ s a -> s{_cdcgaDirectConnectGatewayId = a})

-- | The ID of the virtual private gateway. Example: "vgw-abc123ef" Default: None
cdcgaVirtualGatewayId :: Lens' CreateDirectConnectGatewayAssociation Text
cdcgaVirtualGatewayId = lens _cdcgaVirtualGatewayId (\ s a -> s{_cdcgaVirtualGatewayId = a})

instance AWSRequest
           CreateDirectConnectGatewayAssociation
         where
        type Rs CreateDirectConnectGatewayAssociation =
             CreateDirectConnectGatewayAssociationResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 CreateDirectConnectGatewayAssociationResponse' <$>
                   (x .?> "directConnectGatewayAssociation") <*>
                     (pure (fromEnum s)))

instance Hashable
           CreateDirectConnectGatewayAssociation
         where

instance NFData CreateDirectConnectGatewayAssociation
         where

instance ToHeaders
           CreateDirectConnectGatewayAssociation
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreateDirectConnectGatewayAssociation"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDirectConnectGatewayAssociation
         where
        toJSON CreateDirectConnectGatewayAssociation'{..}
          = object
              (catMaybes
                 [Just
                    ("directConnectGatewayId" .=
                       _cdcgaDirectConnectGatewayId),
                  Just ("virtualGatewayId" .= _cdcgaVirtualGatewayId)])

instance ToPath CreateDirectConnectGatewayAssociation
         where
        toPath = const "/"

instance ToQuery
           CreateDirectConnectGatewayAssociation
         where
        toQuery = const mempty

-- | Container for the response from the CreateDirectConnectGatewayAssociation API call
--
--
--
-- /See:/ 'createDirectConnectGatewayAssociationResponse' smart constructor.
data CreateDirectConnectGatewayAssociationResponse = CreateDirectConnectGatewayAssociationResponse'
  { _cdcgarsDirectConnectGatewayAssociation :: !(Maybe DirectConnectGatewayAssociation)
  , _cdcgarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcgarsDirectConnectGatewayAssociation' - The direct connect gateway association to be created.
--
-- * 'cdcgarsResponseStatus' - -- | The response status code.
createDirectConnectGatewayAssociationResponse
    :: Int -- ^ 'cdcgarsResponseStatus'
    -> CreateDirectConnectGatewayAssociationResponse
createDirectConnectGatewayAssociationResponse pResponseStatus_ =
  CreateDirectConnectGatewayAssociationResponse'
    { _cdcgarsDirectConnectGatewayAssociation = Nothing
    , _cdcgarsResponseStatus = pResponseStatus_
    }


-- | The direct connect gateway association to be created.
cdcgarsDirectConnectGatewayAssociation :: Lens' CreateDirectConnectGatewayAssociationResponse (Maybe DirectConnectGatewayAssociation)
cdcgarsDirectConnectGatewayAssociation = lens _cdcgarsDirectConnectGatewayAssociation (\ s a -> s{_cdcgarsDirectConnectGatewayAssociation = a})

-- | -- | The response status code.
cdcgarsResponseStatus :: Lens' CreateDirectConnectGatewayAssociationResponse Int
cdcgarsResponseStatus = lens _cdcgarsResponseStatus (\ s a -> s{_cdcgarsResponseStatus = a})

instance NFData
           CreateDirectConnectGatewayAssociationResponse
         where
