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
-- Module      : Network.AWS.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network interface. You must detach the network interface before you can delete it.
--
--
module Network.AWS.EC2.DeleteNetworkInterface
    (
    -- * Creating a Request
      deleteNetworkInterface
    , DeleteNetworkInterface
    -- * Request Lenses
    , dninDryRun
    , dninNetworkInterfaceId

    -- * Destructuring the Response
    , deleteNetworkInterfaceResponse
    , DeleteNetworkInterfaceResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteNetworkInterface.
--
--
--
-- /See:/ 'deleteNetworkInterface' smart constructor.
data DeleteNetworkInterface = DeleteNetworkInterface'
  { _dninDryRun             :: !(Maybe Bool)
  , _dninNetworkInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dninDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dninNetworkInterfaceId' - The ID of the network interface.
deleteNetworkInterface
    :: Text -- ^ 'dninNetworkInterfaceId'
    -> DeleteNetworkInterface
deleteNetworkInterface pNetworkInterfaceId_ =
  DeleteNetworkInterface'
    {_dninDryRun = Nothing, _dninNetworkInterfaceId = pNetworkInterfaceId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dninDryRun :: Lens' DeleteNetworkInterface (Maybe Bool)
dninDryRun = lens _dninDryRun (\ s a -> s{_dninDryRun = a})

-- | The ID of the network interface.
dninNetworkInterfaceId :: Lens' DeleteNetworkInterface Text
dninNetworkInterfaceId = lens _dninNetworkInterfaceId (\ s a -> s{_dninNetworkInterfaceId = a})

instance AWSRequest DeleteNetworkInterface where
        type Rs DeleteNetworkInterface =
             DeleteNetworkInterfaceResponse
        request = postQuery ec2
        response
          = receiveNull DeleteNetworkInterfaceResponse'

instance Hashable DeleteNetworkInterface where

instance NFData DeleteNetworkInterface where

instance ToHeaders DeleteNetworkInterface where
        toHeaders = const mempty

instance ToPath DeleteNetworkInterface where
        toPath = const "/"

instance ToQuery DeleteNetworkInterface where
        toQuery DeleteNetworkInterface'{..}
          = mconcat
              ["Action" =:
                 ("DeleteNetworkInterface" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dninDryRun,
               "NetworkInterfaceId" =: _dninNetworkInterfaceId]

-- | /See:/ 'deleteNetworkInterfaceResponse' smart constructor.
data DeleteNetworkInterfaceResponse =
  DeleteNetworkInterfaceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInterfaceResponse' with the minimum fields required to make a request.
--
deleteNetworkInterfaceResponse
    :: DeleteNetworkInterfaceResponse
deleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'


instance NFData DeleteNetworkInterfaceResponse where
