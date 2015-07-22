{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateRouteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified VPC. After you create a route
-- table, you can add routes and associate the table with a subnet.
--
-- For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateRouteTable.html>
module Network.AWS.EC2.CreateRouteTable
    (
    -- * Request
      CreateRouteTable
    -- ** Request constructor
    , createRouteTable
    -- ** Request lenses
    , crtrqDryRun
    , crtrqVPCId

    -- * Response
    , CreateRouteTableResponse
    -- ** Response constructor
    , createRouteTableResponse
    -- ** Response lenses
    , crtrsRouteTable
    , crtrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRouteTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crtrqDryRun'
--
-- * 'crtrqVPCId'
data CreateRouteTable = CreateRouteTable'
    { _crtrqDryRun :: !(Maybe Bool)
    , _crtrqVPCId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRouteTable' smart constructor.
createRouteTable :: Text -> CreateRouteTable
createRouteTable pVPCId =
    CreateRouteTable'
    { _crtrqDryRun = Nothing
    , _crtrqVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
crtrqDryRun :: Lens' CreateRouteTable (Maybe Bool)
crtrqDryRun = lens _crtrqDryRun (\ s a -> s{_crtrqDryRun = a});

-- | The ID of the VPC.
crtrqVPCId :: Lens' CreateRouteTable Text
crtrqVPCId = lens _crtrqVPCId (\ s a -> s{_crtrqVPCId = a});

instance AWSRequest CreateRouteTable where
        type Sv CreateRouteTable = EC2
        type Rs CreateRouteTable = CreateRouteTableResponse
        request = post
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
               "DryRun" =: _crtrqDryRun, "VpcId" =: _crtrqVPCId]

-- | /See:/ 'createRouteTableResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crtrsRouteTable'
--
-- * 'crtrsStatus'
data CreateRouteTableResponse = CreateRouteTableResponse'
    { _crtrsRouteTable :: !(Maybe RouteTable)
    , _crtrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRouteTableResponse' smart constructor.
createRouteTableResponse :: Int -> CreateRouteTableResponse
createRouteTableResponse pStatus =
    CreateRouteTableResponse'
    { _crtrsRouteTable = Nothing
    , _crtrsStatus = pStatus
    }

-- | Information about the route table.
crtrsRouteTable :: Lens' CreateRouteTableResponse (Maybe RouteTable)
crtrsRouteTable = lens _crtrsRouteTable (\ s a -> s{_crtrsRouteTable = a});

-- | FIXME: Undocumented member.
crtrsStatus :: Lens' CreateRouteTableResponse Int
crtrsStatus = lens _crtrsStatus (\ s a -> s{_crtrsStatus = a});
