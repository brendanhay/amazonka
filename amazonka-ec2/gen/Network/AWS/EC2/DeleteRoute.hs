{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified route table.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteRoute.html>
module Network.AWS.EC2.DeleteRoute
    (
    -- * Request
      DeleteRoute
    -- ** Request constructor
    , deleteRoute
    -- ** Request lenses
    , drDryRun
    , drRouteTableId
    , drDestinationCIdRBlock

    -- * Response
    , DeleteRouteResponse
    -- ** Response constructor
    , deleteRouteResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteRoute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drDryRun'
--
-- * 'drRouteTableId'
--
-- * 'drDestinationCIdRBlock'
data DeleteRoute = DeleteRoute'
    { _drDryRun               :: !(Maybe Bool)
    , _drRouteTableId         :: !Text
    , _drDestinationCIdRBlock :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRoute' smart constructor.
deleteRoute :: Text -> Text -> DeleteRoute
deleteRoute pRouteTableId_ pDestinationCIdRBlock_ =
    DeleteRoute'
    { _drDryRun = Nothing
    , _drRouteTableId = pRouteTableId_
    , _drDestinationCIdRBlock = pDestinationCIdRBlock_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
drDryRun :: Lens' DeleteRoute (Maybe Bool)
drDryRun = lens _drDryRun (\ s a -> s{_drDryRun = a});

-- | The ID of the route table.
drRouteTableId :: Lens' DeleteRoute Text
drRouteTableId = lens _drRouteTableId (\ s a -> s{_drRouteTableId = a});

-- | The CIDR range for the route. The value you specify must match the CIDR
-- for the route exactly.
drDestinationCIdRBlock :: Lens' DeleteRoute Text
drDestinationCIdRBlock = lens _drDestinationCIdRBlock (\ s a -> s{_drDestinationCIdRBlock = a});

instance AWSRequest DeleteRoute where
        type Sv DeleteRoute = EC2
        type Rs DeleteRoute = DeleteRouteResponse
        request = post "DeleteRoute"
        response = receiveNull DeleteRouteResponse'

instance ToHeaders DeleteRoute where
        toHeaders = const mempty

instance ToPath DeleteRoute where
        toPath = const "/"

instance ToQuery DeleteRoute where
        toQuery DeleteRoute'{..}
          = mconcat
              ["Action" =: ("DeleteRoute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _drDryRun,
               "RouteTableId" =: _drRouteTableId,
               "DestinationCidrBlock" =: _drDestinationCIdRBlock]

-- | /See:/ 'deleteRouteResponse' smart constructor.
data DeleteRouteResponse =
    DeleteRouteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteRouteResponse' smart constructor.
deleteRouteResponse :: DeleteRouteResponse
deleteRouteResponse = DeleteRouteResponse'
