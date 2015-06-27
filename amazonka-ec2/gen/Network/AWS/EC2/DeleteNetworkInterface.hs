{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified network interface. You must detach the network
-- interface before you can delete it.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkInterface.html>
module Network.AWS.EC2.DeleteNetworkInterface
    (
    -- * Request
      DeleteNetworkInterface
    -- ** Request constructor
    , deleteNetworkInterface
    -- ** Request lenses
    , dni1DryRun
    , dni1NetworkInterfaceId

    -- * Response
    , DeleteNetworkInterfaceResponse
    -- ** Response constructor
    , deleteNetworkInterfaceResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteNetworkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dni1DryRun'
--
-- * 'dni1NetworkInterfaceId'
data DeleteNetworkInterface = DeleteNetworkInterface'
    { _dni1DryRun             :: Maybe Bool
    , _dni1NetworkInterfaceId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteNetworkInterface' smart constructor.
deleteNetworkInterface :: Text -> DeleteNetworkInterface
deleteNetworkInterface pNetworkInterfaceId =
    DeleteNetworkInterface'
    { _dni1DryRun = Nothing
    , _dni1NetworkInterfaceId = pNetworkInterfaceId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dni1DryRun :: Lens' DeleteNetworkInterface (Maybe Bool)
dni1DryRun = lens _dni1DryRun (\ s a -> s{_dni1DryRun = a});

-- | The ID of the network interface.
dni1NetworkInterfaceId :: Lens' DeleteNetworkInterface Text
dni1NetworkInterfaceId = lens _dni1NetworkInterfaceId (\ s a -> s{_dni1NetworkInterfaceId = a});

instance AWSRequest DeleteNetworkInterface where
        type Sv DeleteNetworkInterface = EC2
        type Rs DeleteNetworkInterface =
             DeleteNetworkInterfaceResponse
        request = post
        response
          = receiveNull DeleteNetworkInterfaceResponse'

instance ToHeaders DeleteNetworkInterface where
        toHeaders = const mempty

instance ToPath DeleteNetworkInterface where
        toPath = const "/"

instance ToQuery DeleteNetworkInterface where
        toQuery DeleteNetworkInterface'{..}
          = mconcat
              ["Action" =:
                 ("DeleteNetworkInterface" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dni1DryRun,
               "NetworkInterfaceId" =: _dni1NetworkInterfaceId]

-- | /See:/ 'deleteNetworkInterfaceResponse' smart constructor.
data DeleteNetworkInterfaceResponse =
    DeleteNetworkInterfaceResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteNetworkInterfaceResponse' smart constructor.
deleteNetworkInterfaceResponse :: DeleteNetworkInterfaceResponse
deleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'
