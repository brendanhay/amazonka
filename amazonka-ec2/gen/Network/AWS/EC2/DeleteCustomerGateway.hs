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
-- Module      : Network.AWS.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified customer gateway. You must delete the VPN
-- connection before you can delete the customer gateway.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteCustomerGateway.html AWS API Reference> for DeleteCustomerGateway.
module Network.AWS.EC2.DeleteCustomerGateway
    (
    -- * Creating a Request
      deleteCustomerGateway
    , DeleteCustomerGateway
    -- * Request Lenses
    , dcgcDryRun
    , dcgcCustomerGatewayId

    -- * Destructuring the Response
    , deleteCustomerGatewayResponse
    , DeleteCustomerGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteCustomerGateway' smart constructor.
data DeleteCustomerGateway = DeleteCustomerGateway'
    { _dcgcDryRun            :: !(Maybe Bool)
    , _dcgcCustomerGatewayId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteCustomerGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgcDryRun'
--
-- * 'dcgcCustomerGatewayId'
deleteCustomerGateway
    :: Text -- ^ 'dcgcCustomerGatewayId'
    -> DeleteCustomerGateway
deleteCustomerGateway pCustomerGatewayId_ =
    DeleteCustomerGateway'
    { _dcgcDryRun = Nothing
    , _dcgcCustomerGatewayId = pCustomerGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dcgcDryRun :: Lens' DeleteCustomerGateway (Maybe Bool)
dcgcDryRun = lens _dcgcDryRun (\ s a -> s{_dcgcDryRun = a});

-- | The ID of the customer gateway.
dcgcCustomerGatewayId :: Lens' DeleteCustomerGateway Text
dcgcCustomerGatewayId = lens _dcgcCustomerGatewayId (\ s a -> s{_dcgcCustomerGatewayId = a});

instance AWSRequest DeleteCustomerGateway where
        type Sv DeleteCustomerGateway = EC2
        type Rs DeleteCustomerGateway =
             DeleteCustomerGatewayResponse
        request = postQuery
        response = receiveNull DeleteCustomerGatewayResponse'

instance ToHeaders DeleteCustomerGateway where
        toHeaders = const mempty

instance ToPath DeleteCustomerGateway where
        toPath = const "/"

instance ToQuery DeleteCustomerGateway where
        toQuery DeleteCustomerGateway'{..}
          = mconcat
              ["Action" =: ("DeleteCustomerGateway" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dcgcDryRun,
               "CustomerGatewayId" =: _dcgcCustomerGatewayId]

-- | /See:/ 'deleteCustomerGatewayResponse' smart constructor.
data DeleteCustomerGatewayResponse =
    DeleteCustomerGatewayResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteCustomerGatewayResponse' with the minimum fields required to make a request.
--
deleteCustomerGatewayResponse
    :: DeleteCustomerGatewayResponse
deleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
