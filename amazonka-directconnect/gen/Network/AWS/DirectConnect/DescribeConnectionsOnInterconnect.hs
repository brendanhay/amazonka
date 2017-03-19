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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated in favor of 'DescribeHostedConnections' .
--
--
-- Returns a list of connections that have been provisioned on the given interconnect.
--
module Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
    (
    -- * Creating a Request
      describeConnectionsOnInterconnect
    , DescribeConnectionsOnInterconnect
    -- * Request Lenses
    , dcoiInterconnectId

    -- * Destructuring the Response
    , connections
    , Connections
    -- * Response Lenses
    , cConnections
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DescribeConnectionsOnInterconnect operation.
--
--
--
-- /See:/ 'describeConnectionsOnInterconnect' smart constructor.
newtype DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnect'
    { _dcoiInterconnectId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConnectionsOnInterconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcoiInterconnectId' - ID of the interconnect on which a list of connection is provisioned. Example: dxcon-abc123 Default: None
describeConnectionsOnInterconnect
    :: Text -- ^ 'dcoiInterconnectId'
    -> DescribeConnectionsOnInterconnect
describeConnectionsOnInterconnect pInterconnectId_ =
    DescribeConnectionsOnInterconnect'
    { _dcoiInterconnectId = pInterconnectId_
    }

-- | ID of the interconnect on which a list of connection is provisioned. Example: dxcon-abc123 Default: None
dcoiInterconnectId :: Lens' DescribeConnectionsOnInterconnect Text
dcoiInterconnectId = lens _dcoiInterconnectId (\ s a -> s{_dcoiInterconnectId = a});

instance AWSRequest DescribeConnectionsOnInterconnect
         where
        type Rs DescribeConnectionsOnInterconnect =
             Connections
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeConnectionsOnInterconnect

instance NFData DescribeConnectionsOnInterconnect

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
          = object
              (catMaybes
                 [Just ("interconnectId" .= _dcoiInterconnectId)])

instance ToPath DescribeConnectionsOnInterconnect
         where
        toPath = const "/"

instance ToQuery DescribeConnectionsOnInterconnect
         where
        toQuery = const mempty
