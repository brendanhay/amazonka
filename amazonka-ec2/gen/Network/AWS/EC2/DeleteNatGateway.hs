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
-- Module      : Network.AWS.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a NAT gateway disassociates
-- its Elastic IP address, but does not release the address from your
-- account. Deleting a NAT gateway does not delete any NAT gateway routes
-- in your route tables.
module Network.AWS.EC2.DeleteNatGateway
    (
    -- * Creating a Request
      deleteNatGateway
    , DeleteNatGateway
    -- * Request Lenses
    , dngNatGatewayId

    -- * Destructuring the Response
    , deleteNatGatewayResponse
    , DeleteNatGatewayResponse
    -- * Response Lenses
    , delrsNatGatewayId
    , delrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteNatGateway' smart constructor.
newtype DeleteNatGateway = DeleteNatGateway'
    { _dngNatGatewayId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteNatGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngNatGatewayId'
deleteNatGateway
    :: Text -- ^ 'dngNatGatewayId'
    -> DeleteNatGateway
deleteNatGateway pNatGatewayId_ =
    DeleteNatGateway'
    { _dngNatGatewayId = pNatGatewayId_
    }

-- | The ID of the NAT gateway.
dngNatGatewayId :: Lens' DeleteNatGateway Text
dngNatGatewayId = lens _dngNatGatewayId (\ s a -> s{_dngNatGatewayId = a});

instance AWSRequest DeleteNatGateway where
        type Rs DeleteNatGateway = DeleteNatGatewayResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 DeleteNatGatewayResponse' <$>
                   (x .@? "natGatewayId") <*> (pure (fromEnum s)))

instance Hashable DeleteNatGateway

instance ToHeaders DeleteNatGateway where
        toHeaders = const mempty

instance ToPath DeleteNatGateway where
        toPath = const "/"

instance ToQuery DeleteNatGateway where
        toQuery DeleteNatGateway'{..}
          = mconcat
              ["Action" =: ("DeleteNatGateway" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "NatGatewayId" =: _dngNatGatewayId]

-- | /See:/ 'deleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'
    { _delrsNatGatewayId   :: !(Maybe Text)
    , _delrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteNatGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsNatGatewayId'
--
-- * 'delrsResponseStatus'
deleteNatGatewayResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteNatGatewayResponse
deleteNatGatewayResponse pResponseStatus_ =
    DeleteNatGatewayResponse'
    { _delrsNatGatewayId = Nothing
    , _delrsResponseStatus = pResponseStatus_
    }

-- | The ID of the NAT gateway.
delrsNatGatewayId :: Lens' DeleteNatGatewayResponse (Maybe Text)
delrsNatGatewayId = lens _delrsNatGatewayId (\ s a -> s{_delrsNatGatewayId = a});

-- | The response status code.
delrsResponseStatus :: Lens' DeleteNatGatewayResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});
