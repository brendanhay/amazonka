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
-- Module      : Network.AWS.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified VPC. After you create a route
-- table, you can add routes and associate the table with a subnet.
--
-- For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRouteTable.html AWS API Reference> for CreateRouteTable.
module Network.AWS.EC2.CreateRouteTable
    (
    -- * Creating a Request
      createRouteTable
    , CreateRouteTable
    -- * Request Lenses
    , crtDryRun
    , crtVPCId

    -- * Destructuring the Response
    , createRouteTableResponse
    , CreateRouteTableResponse
    -- * Response Lenses
    , crtrsRouteTable
    , crtrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRouteTable' smart constructor.
data CreateRouteTable = CreateRouteTable'
    { _crtDryRun :: !(Maybe Bool)
    , _crtVPCId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtDryRun'
--
-- * 'crtVPCId'
createRouteTable
    :: Text -- ^ 'crtVPCId'
    -> CreateRouteTable
createRouteTable pVPCId_ =
    CreateRouteTable'
    { _crtDryRun = Nothing
    , _crtVPCId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
crtDryRun :: Lens' CreateRouteTable (Maybe Bool)
crtDryRun = lens _crtDryRun (\ s a -> s{_crtDryRun = a});

-- | The ID of the VPC.
crtVPCId :: Lens' CreateRouteTable Text
crtVPCId = lens _crtVPCId (\ s a -> s{_crtVPCId = a});

instance AWSRequest CreateRouteTable where
        type Rs CreateRouteTable = CreateRouteTableResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 CreateRouteTableResponse' <$>
                   (x .@? "routeTable") <*> (pure (fromEnum s)))

instance ToHeaders CreateRouteTable where
        toHeaders = const mempty

instance ToPath CreateRouteTable where
        toPath = const "/"

instance ToQuery CreateRouteTable where
        toQuery CreateRouteTable'{..}
          = mconcat
              ["Action" =: ("CreateRouteTable" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _crtDryRun, "VpcId" =: _crtVPCId]

-- | /See:/ 'createRouteTableResponse' smart constructor.
data CreateRouteTableResponse = CreateRouteTableResponse'
    { _crtrsRouteTable :: !(Maybe RouteTable)
    , _crtrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRouteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtrsRouteTable'
--
-- * 'crtrsStatus'
createRouteTableResponse
    :: Int -- ^ 'crtrsStatus'
    -> CreateRouteTableResponse
createRouteTableResponse pStatus_ =
    CreateRouteTableResponse'
    { _crtrsRouteTable = Nothing
    , _crtrsStatus = pStatus_
    }

-- | Information about the route table.
crtrsRouteTable :: Lens' CreateRouteTableResponse (Maybe RouteTable)
crtrsRouteTable = lens _crtrsRouteTable (\ s a -> s{_crtrsRouteTable = a});

-- | The response status code.
crtrsStatus :: Lens' CreateRouteTableResponse Int
crtrsStatus = lens _crtrsStatus (\ s a -> s{_crtrsStatus = a});
