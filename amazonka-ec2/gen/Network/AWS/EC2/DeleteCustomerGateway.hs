{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified customer gateway. You must delete the VPN
-- connection before you can delete the customer gateway.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteCustomerGateway.html>
module Network.AWS.EC2.DeleteCustomerGateway
    (
    -- * Request
      DeleteCustomerGateway
    -- ** Request constructor
    , deleteCustomerGateway
    -- ** Request lenses
    , dcg1DryRun
    , dcg1CustomerGatewayId

    -- * Response
    , DeleteCustomerGatewayResponse
    -- ** Response constructor
    , deleteCustomerGatewayResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteCustomerGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcg1DryRun'
--
-- * 'dcg1CustomerGatewayId'
data DeleteCustomerGateway = DeleteCustomerGateway'
    { _dcg1DryRun            :: !(Maybe Bool)
    , _dcg1CustomerGatewayId :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteCustomerGateway' smart constructor.
deleteCustomerGateway :: Text -> DeleteCustomerGateway
deleteCustomerGateway pCustomerGatewayId =
    DeleteCustomerGateway'
    { _dcg1DryRun = Nothing
    , _dcg1CustomerGatewayId = pCustomerGatewayId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dcg1DryRun :: Lens' DeleteCustomerGateway (Maybe Bool)
dcg1DryRun = lens _dcg1DryRun (\ s a -> s{_dcg1DryRun = a});

-- | The ID of the customer gateway.
dcg1CustomerGatewayId :: Lens' DeleteCustomerGateway Text
dcg1CustomerGatewayId = lens _dcg1CustomerGatewayId (\ s a -> s{_dcg1CustomerGatewayId = a});

instance AWSRequest DeleteCustomerGateway where
        type Sv DeleteCustomerGateway = EC2
        type Rs DeleteCustomerGateway =
             DeleteCustomerGatewayResponse
        request = post
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
               "DryRun" =: _dcg1DryRun,
               "CustomerGatewayId" =: _dcg1CustomerGatewayId]

-- | /See:/ 'deleteCustomerGatewayResponse' smart constructor.
data DeleteCustomerGatewayResponse =
    DeleteCustomerGatewayResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteCustomerGatewayResponse' smart constructor.
deleteCustomerGatewayResponse :: DeleteCustomerGatewayResponse
deleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
