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
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between the specified Direct Connect gateway and virtual private gateway.
--
--
-- We recommend that you specify the @associationID@ to delete the association. Alternatively, if you own virtual gateway and a Direct Connect gateway association, you can specify the @virtualGatewayId@ and @directConnectGatewayId@ to delete an association.
module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
  ( -- * Creating a Request
    deleteDirectConnectGatewayAssociation,
    DeleteDirectConnectGatewayAssociation,

    -- * Request Lenses
    delVirtualGatewayId,
    delAssociationId,
    delDirectConnectGatewayId,

    -- * Destructuring the Response
    deleteDirectConnectGatewayAssociationResponse,
    DeleteDirectConnectGatewayAssociationResponse,

    -- * Response Lenses
    delrsDirectConnectGatewayAssociation,
    delrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDirectConnectGatewayAssociation' smart constructor.
data DeleteDirectConnectGatewayAssociation = DeleteDirectConnectGatewayAssociation'
  { _delVirtualGatewayId ::
      !(Maybe Text),
    _delAssociationId ::
      !(Maybe Text),
    _delDirectConnectGatewayId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delVirtualGatewayId' - The ID of the virtual private gateway.
--
-- * 'delAssociationId' - The ID of the Direct Connect gateway association.
--
-- * 'delDirectConnectGatewayId' - The ID of the Direct Connect gateway.
deleteDirectConnectGatewayAssociation ::
  DeleteDirectConnectGatewayAssociation
deleteDirectConnectGatewayAssociation =
  DeleteDirectConnectGatewayAssociation'
    { _delVirtualGatewayId =
        Nothing,
      _delAssociationId = Nothing,
      _delDirectConnectGatewayId = Nothing
    }

-- | The ID of the virtual private gateway.
delVirtualGatewayId :: Lens' DeleteDirectConnectGatewayAssociation (Maybe Text)
delVirtualGatewayId = lens _delVirtualGatewayId (\s a -> s {_delVirtualGatewayId = a})

-- | The ID of the Direct Connect gateway association.
delAssociationId :: Lens' DeleteDirectConnectGatewayAssociation (Maybe Text)
delAssociationId = lens _delAssociationId (\s a -> s {_delAssociationId = a})

-- | The ID of the Direct Connect gateway.
delDirectConnectGatewayId :: Lens' DeleteDirectConnectGatewayAssociation (Maybe Text)
delDirectConnectGatewayId = lens _delDirectConnectGatewayId (\s a -> s {_delDirectConnectGatewayId = a})

instance AWSRequest DeleteDirectConnectGatewayAssociation where
  type
    Rs DeleteDirectConnectGatewayAssociation =
      DeleteDirectConnectGatewayAssociationResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationResponse'
            <$> (x .?> "directConnectGatewayAssociation") <*> (pure (fromEnum s))
      )

instance Hashable DeleteDirectConnectGatewayAssociation

instance NFData DeleteDirectConnectGatewayAssociation

instance ToHeaders DeleteDirectConnectGatewayAssociation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "OvertureService.DeleteDirectConnectGatewayAssociation" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDirectConnectGatewayAssociation where
  toJSON DeleteDirectConnectGatewayAssociation' {..} =
    object
      ( catMaybes
          [ ("virtualGatewayId" .=) <$> _delVirtualGatewayId,
            ("associationId" .=) <$> _delAssociationId,
            ("directConnectGatewayId" .=) <$> _delDirectConnectGatewayId
          ]
      )

instance ToPath DeleteDirectConnectGatewayAssociation where
  toPath = const "/"

instance ToQuery DeleteDirectConnectGatewayAssociation where
  toQuery = const mempty

-- | /See:/ 'deleteDirectConnectGatewayAssociationResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationResponse = DeleteDirectConnectGatewayAssociationResponse'
  { _delrsDirectConnectGatewayAssociation ::
      !( Maybe
           DirectConnectGatewayAssociation
       ),
    _delrsResponseStatus ::
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

-- | Creates a value of 'DeleteDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsDirectConnectGatewayAssociation' - Information about the deleted association.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteDirectConnectGatewayAssociationResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteDirectConnectGatewayAssociationResponse
deleteDirectConnectGatewayAssociationResponse pResponseStatus_ =
  DeleteDirectConnectGatewayAssociationResponse'
    { _delrsDirectConnectGatewayAssociation =
        Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | Information about the deleted association.
delrsDirectConnectGatewayAssociation :: Lens' DeleteDirectConnectGatewayAssociationResponse (Maybe DirectConnectGatewayAssociation)
delrsDirectConnectGatewayAssociation = lens _delrsDirectConnectGatewayAssociation (\s a -> s {_delrsDirectConnectGatewayAssociation = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteDirectConnectGatewayAssociationResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteDirectConnectGatewayAssociationResponse
