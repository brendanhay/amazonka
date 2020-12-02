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
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a direct connect gateway. You must first delete all virtual interfaces that are attached to the direct connect gateway and disassociate all virtual private gateways that are associated with the direct connect gateway.
--
--
module Network.AWS.DirectConnect.DeleteDirectConnectGateway
    (
    -- * Creating a Request
      deleteDirectConnectGateway
    , DeleteDirectConnectGateway
    -- * Request Lenses
    , ddcgdDirectConnectGatewayId

    -- * Destructuring the Response
    , deleteDirectConnectGatewayResponse
    , DeleteDirectConnectGatewayResponse
    -- * Response Lenses
    , ddcgdrsDirectConnectGateway
    , ddcgdrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DeleteDirectConnectGateway operation.
--
--
--
-- /See:/ 'deleteDirectConnectGateway' smart constructor.
newtype DeleteDirectConnectGateway = DeleteDirectConnectGateway'
  { _ddcgdDirectConnectGatewayId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectConnectGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgdDirectConnectGatewayId' - The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
deleteDirectConnectGateway
    :: Text -- ^ 'ddcgdDirectConnectGatewayId'
    -> DeleteDirectConnectGateway
deleteDirectConnectGateway pDirectConnectGatewayId_ =
  DeleteDirectConnectGateway'
    {_ddcgdDirectConnectGatewayId = pDirectConnectGatewayId_}


-- | The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
ddcgdDirectConnectGatewayId :: Lens' DeleteDirectConnectGateway Text
ddcgdDirectConnectGatewayId = lens _ddcgdDirectConnectGatewayId (\ s a -> s{_ddcgdDirectConnectGatewayId = a})

instance AWSRequest DeleteDirectConnectGateway where
        type Rs DeleteDirectConnectGateway =
             DeleteDirectConnectGatewayResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDirectConnectGatewayResponse' <$>
                   (x .?> "directConnectGateway") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteDirectConnectGateway where

instance NFData DeleteDirectConnectGateway where

instance ToHeaders DeleteDirectConnectGateway where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteDirectConnectGateway" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDirectConnectGateway where
        toJSON DeleteDirectConnectGateway'{..}
          = object
              (catMaybes
                 [Just
                    ("directConnectGatewayId" .=
                       _ddcgdDirectConnectGatewayId)])

instance ToPath DeleteDirectConnectGateway where
        toPath = const "/"

instance ToQuery DeleteDirectConnectGateway where
        toQuery = const mempty

-- | Container for the response from the DeleteDirectConnectGateway API call
--
--
--
-- /See:/ 'deleteDirectConnectGatewayResponse' smart constructor.
data DeleteDirectConnectGatewayResponse = DeleteDirectConnectGatewayResponse'
  { _ddcgdrsDirectConnectGateway :: !(Maybe DirectConnectGateway)
  , _ddcgdrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectConnectGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgdrsDirectConnectGateway' - The direct connect gateway to be deleted.
--
-- * 'ddcgdrsResponseStatus' - -- | The response status code.
deleteDirectConnectGatewayResponse
    :: Int -- ^ 'ddcgdrsResponseStatus'
    -> DeleteDirectConnectGatewayResponse
deleteDirectConnectGatewayResponse pResponseStatus_ =
  DeleteDirectConnectGatewayResponse'
    { _ddcgdrsDirectConnectGateway = Nothing
    , _ddcgdrsResponseStatus = pResponseStatus_
    }


-- | The direct connect gateway to be deleted.
ddcgdrsDirectConnectGateway :: Lens' DeleteDirectConnectGatewayResponse (Maybe DirectConnectGateway)
ddcgdrsDirectConnectGateway = lens _ddcgdrsDirectConnectGateway (\ s a -> s{_ddcgdrsDirectConnectGateway = a})

-- | -- | The response status code.
ddcgdrsResponseStatus :: Lens' DeleteDirectConnectGatewayResponse Int
ddcgdrsResponseStatus = lens _ddcgdrsResponseStatus (\ s a -> s{_ddcgdrsResponseStatus = a})

instance NFData DeleteDirectConnectGatewayResponse
         where
