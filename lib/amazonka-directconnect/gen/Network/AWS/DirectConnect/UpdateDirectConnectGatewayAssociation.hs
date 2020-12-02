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
-- Module      : Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the Direct Connect gateway association.
--
--
-- Add or remove prefixes from the association.
module Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
  ( -- * Creating a Request
    updateDirectConnectGatewayAssociation,
    UpdateDirectConnectGatewayAssociation,

    -- * Request Lenses
    udcgaAssociationId,
    udcgaAddAllowedPrefixesToDirectConnectGateway,
    udcgaRemoveAllowedPrefixesToDirectConnectGateway,

    -- * Destructuring the Response
    updateDirectConnectGatewayAssociationResponse,
    UpdateDirectConnectGatewayAssociationResponse,

    -- * Response Lenses
    udcgarsDirectConnectGatewayAssociation,
    udcgarsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDirectConnectGatewayAssociation' smart constructor.
data UpdateDirectConnectGatewayAssociation = UpdateDirectConnectGatewayAssociation'
  { _udcgaAssociationId ::
      !(Maybe Text),
    _udcgaAddAllowedPrefixesToDirectConnectGateway ::
      !( Maybe
           [RouteFilterPrefix]
       ),
    _udcgaRemoveAllowedPrefixesToDirectConnectGateway ::
      !( Maybe
           [RouteFilterPrefix]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcgaAssociationId' - The ID of the Direct Connect gateway association.
--
-- * 'udcgaAddAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- * 'udcgaRemoveAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
updateDirectConnectGatewayAssociation ::
  UpdateDirectConnectGatewayAssociation
updateDirectConnectGatewayAssociation =
  UpdateDirectConnectGatewayAssociation'
    { _udcgaAssociationId =
        Nothing,
      _udcgaAddAllowedPrefixesToDirectConnectGateway = Nothing,
      _udcgaRemoveAllowedPrefixesToDirectConnectGateway =
        Nothing
    }

-- | The ID of the Direct Connect gateway association.
udcgaAssociationId :: Lens' UpdateDirectConnectGatewayAssociation (Maybe Text)
udcgaAssociationId = lens _udcgaAssociationId (\s a -> s {_udcgaAssociationId = a})

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
udcgaAddAllowedPrefixesToDirectConnectGateway :: Lens' UpdateDirectConnectGatewayAssociation [RouteFilterPrefix]
udcgaAddAllowedPrefixesToDirectConnectGateway = lens _udcgaAddAllowedPrefixesToDirectConnectGateway (\s a -> s {_udcgaAddAllowedPrefixesToDirectConnectGateway = a}) . _Default . _Coerce

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
udcgaRemoveAllowedPrefixesToDirectConnectGateway :: Lens' UpdateDirectConnectGatewayAssociation [RouteFilterPrefix]
udcgaRemoveAllowedPrefixesToDirectConnectGateway = lens _udcgaRemoveAllowedPrefixesToDirectConnectGateway (\s a -> s {_udcgaRemoveAllowedPrefixesToDirectConnectGateway = a}) . _Default . _Coerce

instance AWSRequest UpdateDirectConnectGatewayAssociation where
  type
    Rs UpdateDirectConnectGatewayAssociation =
      UpdateDirectConnectGatewayAssociationResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          UpdateDirectConnectGatewayAssociationResponse'
            <$> (x .?> "directConnectGatewayAssociation") <*> (pure (fromEnum s))
      )

instance Hashable UpdateDirectConnectGatewayAssociation

instance NFData UpdateDirectConnectGatewayAssociation

instance ToHeaders UpdateDirectConnectGatewayAssociation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "OvertureService.UpdateDirectConnectGatewayAssociation" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateDirectConnectGatewayAssociation where
  toJSON UpdateDirectConnectGatewayAssociation' {..} =
    object
      ( catMaybes
          [ ("associationId" .=) <$> _udcgaAssociationId,
            ("addAllowedPrefixesToDirectConnectGateway" .=)
              <$> _udcgaAddAllowedPrefixesToDirectConnectGateway,
            ("removeAllowedPrefixesToDirectConnectGateway" .=)
              <$> _udcgaRemoveAllowedPrefixesToDirectConnectGateway
          ]
      )

instance ToPath UpdateDirectConnectGatewayAssociation where
  toPath = const "/"

instance ToQuery UpdateDirectConnectGatewayAssociation where
  toQuery = const mempty

-- | /See:/ 'updateDirectConnectGatewayAssociationResponse' smart constructor.
data UpdateDirectConnectGatewayAssociationResponse = UpdateDirectConnectGatewayAssociationResponse'
  { _udcgarsDirectConnectGatewayAssociation ::
      !( Maybe
           DirectConnectGatewayAssociation
       ),
    _udcgarsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcgarsDirectConnectGatewayAssociation' - Undocumented member.
--
-- * 'udcgarsResponseStatus' - -- | The response status code.
updateDirectConnectGatewayAssociationResponse ::
  -- | 'udcgarsResponseStatus'
  Int ->
  UpdateDirectConnectGatewayAssociationResponse
updateDirectConnectGatewayAssociationResponse pResponseStatus_ =
  UpdateDirectConnectGatewayAssociationResponse'
    { _udcgarsDirectConnectGatewayAssociation =
        Nothing,
      _udcgarsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
udcgarsDirectConnectGatewayAssociation :: Lens' UpdateDirectConnectGatewayAssociationResponse (Maybe DirectConnectGatewayAssociation)
udcgarsDirectConnectGatewayAssociation = lens _udcgarsDirectConnectGatewayAssociation (\s a -> s {_udcgarsDirectConnectGatewayAssociation = a})

-- | -- | The response status code.
udcgarsResponseStatus :: Lens' UpdateDirectConnectGatewayAssociationResponse Int
udcgarsResponseStatus = lens _udcgarsResponseStatus (\s a -> s {_udcgarsResponseStatus = a})

instance NFData UpdateDirectConnectGatewayAssociationResponse
