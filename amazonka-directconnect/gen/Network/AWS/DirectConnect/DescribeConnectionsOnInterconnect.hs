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
-- Module      : Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of connections that have been provisioned on the given
-- interconnect.
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnectionsOnInterconnect.html AWS API Reference> for DescribeConnectionsOnInterconnect.
module Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
    (
    -- * Creating a Request
      DescribeConnectionsOnInterconnect
    , describeConnectionsOnInterconnect
    -- * Request Lenses
    , dcoiInterconnectId

    -- * Destructuring the Response
    , Connections
    , connections
    -- * Response Lenses
    , cConnections
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DescribeConnectionsOnInterconnect
-- operation.
--
-- /See:/ 'describeConnectionsOnInterconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcoiInterconnectId'
newtype DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnect'
    { _dcoiInterconnectId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConnectionsOnInterconnect' smart constructor.
describeConnectionsOnInterconnect :: Text -> DescribeConnectionsOnInterconnect
describeConnectionsOnInterconnect pInterconnectId_ =
    DescribeConnectionsOnInterconnect'
    { _dcoiInterconnectId = pInterconnectId_
    }

-- | ID of the interconnect on which a list of connection is provisioned.
--
-- Example: dxcon-abc123
--
-- Default: None
dcoiInterconnectId :: Lens' DescribeConnectionsOnInterconnect Text
dcoiInterconnectId = lens _dcoiInterconnectId (\ s a -> s{_dcoiInterconnectId = a});

instance AWSRequest DescribeConnectionsOnInterconnect
         where
        type Sv DescribeConnectionsOnInterconnect =
             DirectConnect
        type Rs DescribeConnectionsOnInterconnect =
             Connections
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders DescribeConnectionsOnInterconnect
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeConnectionsOnInterconnect"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConnectionsOnInterconnect
         where
        toJSON DescribeConnectionsOnInterconnect'{..}
          = object ["interconnectId" .= _dcoiInterconnectId]

instance ToPath DescribeConnectionsOnInterconnect
         where
        toPath = const "/"

instance ToQuery DescribeConnectionsOnInterconnect
         where
        toQuery = const mempty
