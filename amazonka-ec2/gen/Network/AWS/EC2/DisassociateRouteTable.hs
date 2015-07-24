{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateRouteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a subnet from a route table.
--
-- After you perform this action, the subnet no longer uses the routes in
-- the route table. Instead, it uses the routes in the VPC\'s main route
-- table. For more information about route tables, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateRouteTable.html>
module Network.AWS.EC2.DisassociateRouteTable
    (
    -- * Request
      DisassociateRouteTable
    -- ** Request constructor
    , disassociateRouteTable
    -- ** Request lenses
    , drtDryRun
    , drtAssociationId

    -- * Response
    , DisassociateRouteTableResponse
    -- ** Response constructor
    , disassociateRouteTableResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disassociateRouteTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drtDryRun'
--
-- * 'drtAssociationId'
data DisassociateRouteTable = DisassociateRouteTable'
    { _drtDryRun        :: !(Maybe Bool)
    , _drtAssociationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateRouteTable' smart constructor.
disassociateRouteTable :: Text -> DisassociateRouteTable
disassociateRouteTable pAssociationId_ =
    DisassociateRouteTable'
    { _drtDryRun = Nothing
    , _drtAssociationId = pAssociationId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
drtDryRun :: Lens' DisassociateRouteTable (Maybe Bool)
drtDryRun = lens _drtDryRun (\ s a -> s{_drtDryRun = a});

-- | The association ID representing the current association between the
-- route table and subnet.
drtAssociationId :: Lens' DisassociateRouteTable Text
drtAssociationId = lens _drtAssociationId (\ s a -> s{_drtAssociationId = a});

instance AWSRequest DisassociateRouteTable where
        type Sv DisassociateRouteTable = EC2
        type Rs DisassociateRouteTable =
             DisassociateRouteTableResponse
        request = post "DisassociateRouteTable"
        response
          = receiveNull DisassociateRouteTableResponse'

instance ToHeaders DisassociateRouteTable where
        toHeaders = const mempty

instance ToPath DisassociateRouteTable where
        toPath = const "/"

instance ToQuery DisassociateRouteTable where
        toQuery DisassociateRouteTable'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateRouteTable" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _drtDryRun,
               "AssociationId" =: _drtAssociationId]

-- | /See:/ 'disassociateRouteTableResponse' smart constructor.
data DisassociateRouteTableResponse =
    DisassociateRouteTableResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateRouteTableResponse' smart constructor.
disassociateRouteTableResponse :: DisassociateRouteTableResponse
disassociateRouteTableResponse = DisassociateRouteTableResponse'
