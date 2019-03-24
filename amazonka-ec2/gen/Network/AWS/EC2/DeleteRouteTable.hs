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
-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route table. You must disassociate the route table from any subnets before you can delete it. You can't delete the main route table.
--
--
module Network.AWS.EC2.DeleteRouteTable
    (
    -- * Creating a Request
      deleteRouteTable
    , DeleteRouteTable
    -- * Request Lenses
    , drtrDryRun
    , drtrRouteTableId

    -- * Destructuring the Response
    , deleteRouteTableResponse
    , DeleteRouteTableResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRouteTable' smart constructor.
data DeleteRouteTable = DeleteRouteTable'
  { _drtrDryRun       :: !(Maybe Bool)
  , _drtrRouteTableId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'drtrRouteTableId' - The ID of the route table.
deleteRouteTable
    :: Text -- ^ 'drtrRouteTableId'
    -> DeleteRouteTable
deleteRouteTable pRouteTableId_ =
  DeleteRouteTable' {_drtrDryRun = Nothing, _drtrRouteTableId = pRouteTableId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
drtrDryRun :: Lens' DeleteRouteTable (Maybe Bool)
drtrDryRun = lens _drtrDryRun (\ s a -> s{_drtrDryRun = a})

-- | The ID of the route table.
drtrRouteTableId :: Lens' DeleteRouteTable Text
drtrRouteTableId = lens _drtrRouteTableId (\ s a -> s{_drtrRouteTableId = a})

instance AWSRequest DeleteRouteTable where
        type Rs DeleteRouteTable = DeleteRouteTableResponse
        request = postQuery ec2
        response = receiveNull DeleteRouteTableResponse'

instance Hashable DeleteRouteTable where

instance NFData DeleteRouteTable where

instance ToHeaders DeleteRouteTable where
        toHeaders = const mempty

instance ToPath DeleteRouteTable where
        toPath = const "/"

instance ToQuery DeleteRouteTable where
        toQuery DeleteRouteTable'{..}
          = mconcat
              ["Action" =: ("DeleteRouteTable" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _drtrDryRun,
               "RouteTableId" =: _drtrRouteTableId]

-- | /See:/ 'deleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse =
  DeleteRouteTableResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRouteTableResponse' with the minimum fields required to make a request.
--
deleteRouteTableResponse
    :: DeleteRouteTableResponse
deleteRouteTableResponse = DeleteRouteTableResponse'


instance NFData DeleteRouteTableResponse where
