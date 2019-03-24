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
-- Module      : Network.AWS.EC2.DeleteInternetGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified internet gateway. You must detach the internet gateway from the VPC before you can delete it.
--
--
module Network.AWS.EC2.DeleteInternetGateway
    (
    -- * Creating a Request
      deleteInternetGateway
    , DeleteInternetGateway
    -- * Request Lenses
    , digiDryRun
    , digiInternetGatewayId

    -- * Destructuring the Response
    , deleteInternetGatewayResponse
    , DeleteInternetGatewayResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInternetGateway' smart constructor.
data DeleteInternetGateway = DeleteInternetGateway'
  { _digiDryRun            :: !(Maybe Bool)
  , _digiInternetGatewayId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'digiInternetGatewayId' - The ID of the internet gateway.
deleteInternetGateway
    :: Text -- ^ 'digiInternetGatewayId'
    -> DeleteInternetGateway
deleteInternetGateway pInternetGatewayId_ =
  DeleteInternetGateway'
    {_digiDryRun = Nothing, _digiInternetGatewayId = pInternetGatewayId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
digiDryRun :: Lens' DeleteInternetGateway (Maybe Bool)
digiDryRun = lens _digiDryRun (\ s a -> s{_digiDryRun = a})

-- | The ID of the internet gateway.
digiInternetGatewayId :: Lens' DeleteInternetGateway Text
digiInternetGatewayId = lens _digiInternetGatewayId (\ s a -> s{_digiInternetGatewayId = a})

instance AWSRequest DeleteInternetGateway where
        type Rs DeleteInternetGateway =
             DeleteInternetGatewayResponse
        request = postQuery ec2
        response = receiveNull DeleteInternetGatewayResponse'

instance Hashable DeleteInternetGateway where

instance NFData DeleteInternetGateway where

instance ToHeaders DeleteInternetGateway where
        toHeaders = const mempty

instance ToPath DeleteInternetGateway where
        toPath = const "/"

instance ToQuery DeleteInternetGateway where
        toQuery DeleteInternetGateway'{..}
          = mconcat
              ["Action" =: ("DeleteInternetGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _digiDryRun,
               "InternetGatewayId" =: _digiInternetGatewayId]

-- | /See:/ 'deleteInternetGatewayResponse' smart constructor.
data DeleteInternetGatewayResponse =
  DeleteInternetGatewayResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInternetGatewayResponse' with the minimum fields required to make a request.
--
deleteInternetGatewayResponse
    :: DeleteInternetGatewayResponse
deleteInternetGatewayResponse = DeleteInternetGatewayResponse'


instance NFData DeleteInternetGatewayResponse where
