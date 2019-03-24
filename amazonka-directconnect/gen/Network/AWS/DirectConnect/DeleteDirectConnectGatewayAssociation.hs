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
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between the specified Direct Connect gateway and virtual private gateway.
--
--
module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
    (
    -- * Creating a Request
      deleteDirectConnectGatewayAssociation
    , DeleteDirectConnectGatewayAssociation
    -- * Request Lenses
    , delDirectConnectGatewayId
    , delVirtualGatewayId

    -- * Destructuring the Response
    , deleteDirectConnectGatewayAssociationResponse
    , DeleteDirectConnectGatewayAssociationResponse
    -- * Response Lenses
    , delrsDirectConnectGatewayAssociation
    , delrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDirectConnectGatewayAssociation' smart constructor.
data DeleteDirectConnectGatewayAssociation = DeleteDirectConnectGatewayAssociation'
  { _delDirectConnectGatewayId :: !Text
  , _delVirtualGatewayId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'delVirtualGatewayId' - The ID of the virtual private gateway.
deleteDirectConnectGatewayAssociation
    :: Text -- ^ 'delDirectConnectGatewayId'
    -> Text -- ^ 'delVirtualGatewayId'
    -> DeleteDirectConnectGatewayAssociation
deleteDirectConnectGatewayAssociation pDirectConnectGatewayId_ pVirtualGatewayId_ =
  DeleteDirectConnectGatewayAssociation'
    { _delDirectConnectGatewayId = pDirectConnectGatewayId_
    , _delVirtualGatewayId = pVirtualGatewayId_
    }


-- | The ID of the Direct Connect gateway.
delDirectConnectGatewayId :: Lens' DeleteDirectConnectGatewayAssociation Text
delDirectConnectGatewayId = lens _delDirectConnectGatewayId (\ s a -> s{_delDirectConnectGatewayId = a})

-- | The ID of the virtual private gateway.
delVirtualGatewayId :: Lens' DeleteDirectConnectGatewayAssociation Text
delVirtualGatewayId = lens _delVirtualGatewayId (\ s a -> s{_delVirtualGatewayId = a})

instance AWSRequest
           DeleteDirectConnectGatewayAssociation
         where
        type Rs DeleteDirectConnectGatewayAssociation =
             DeleteDirectConnectGatewayAssociationResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDirectConnectGatewayAssociationResponse' <$>
                   (x .?> "directConnectGatewayAssociation") <*>
                     (pure (fromEnum s)))

instance Hashable
           DeleteDirectConnectGatewayAssociation
         where

instance NFData DeleteDirectConnectGatewayAssociation
         where

instance ToHeaders
           DeleteDirectConnectGatewayAssociation
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteDirectConnectGatewayAssociation"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDirectConnectGatewayAssociation
         where
        toJSON DeleteDirectConnectGatewayAssociation'{..}
          = object
              (catMaybes
                 [Just
                    ("directConnectGatewayId" .=
                       _delDirectConnectGatewayId),
                  Just ("virtualGatewayId" .= _delVirtualGatewayId)])

instance ToPath DeleteDirectConnectGatewayAssociation
         where
        toPath = const "/"

instance ToQuery
           DeleteDirectConnectGatewayAssociation
         where
        toQuery = const mempty

-- | /See:/ 'deleteDirectConnectGatewayAssociationResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationResponse = DeleteDirectConnectGatewayAssociationResponse'
  { _delrsDirectConnectGatewayAssociation :: !(Maybe DirectConnectGatewayAssociation)
  , _delrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsDirectConnectGatewayAssociation' - The association to be deleted.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteDirectConnectGatewayAssociationResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteDirectConnectGatewayAssociationResponse
deleteDirectConnectGatewayAssociationResponse pResponseStatus_ =
  DeleteDirectConnectGatewayAssociationResponse'
    { _delrsDirectConnectGatewayAssociation = Nothing
    , _delrsResponseStatus = pResponseStatus_
    }


-- | The association to be deleted.
delrsDirectConnectGatewayAssociation :: Lens' DeleteDirectConnectGatewayAssociationResponse (Maybe DirectConnectGatewayAssociation)
delrsDirectConnectGatewayAssociation = lens _delrsDirectConnectGatewayAssociation (\ s a -> s{_delrsDirectConnectGatewayAssociation = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteDirectConnectGatewayAssociationResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData
           DeleteDirectConnectGatewayAssociationResponse
         where
