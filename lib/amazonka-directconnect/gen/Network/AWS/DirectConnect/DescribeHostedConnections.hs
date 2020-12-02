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
-- Module      : Network.AWS.DirectConnect.DescribeHostedConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of hosted connections that have been provisioned on the given interconnect or link aggregation group (LAG).
--
--
module Network.AWS.DirectConnect.DescribeHostedConnections
    (
    -- * Creating a Request
      describeHostedConnections
    , DescribeHostedConnections
    -- * Request Lenses
    , dhcConnectionId

    -- * Destructuring the Response
    , connections
    , Connections
    -- * Response Lenses
    , cConnections
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DescribeHostedConnections operation.
--
--
--
-- /See:/ 'describeHostedConnections' smart constructor.
newtype DescribeHostedConnections = DescribeHostedConnections'
  { _dhcConnectionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHostedConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcConnectionId' - The ID of the interconnect or LAG on which the hosted connections are provisioned. Example: dxcon-abc123 or dxlag-abc123 Default: None
describeHostedConnections
    :: Text -- ^ 'dhcConnectionId'
    -> DescribeHostedConnections
describeHostedConnections pConnectionId_ =
  DescribeHostedConnections' {_dhcConnectionId = pConnectionId_}


-- | The ID of the interconnect or LAG on which the hosted connections are provisioned. Example: dxcon-abc123 or dxlag-abc123 Default: None
dhcConnectionId :: Lens' DescribeHostedConnections Text
dhcConnectionId = lens _dhcConnectionId (\ s a -> s{_dhcConnectionId = a})

instance AWSRequest DescribeHostedConnections where
        type Rs DescribeHostedConnections = Connections
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeHostedConnections where

instance NFData DescribeHostedConnections where

instance ToHeaders DescribeHostedConnections where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeHostedConnections" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeHostedConnections where
        toJSON DescribeHostedConnections'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _dhcConnectionId)])

instance ToPath DescribeHostedConnections where
        toPath = const "/"

instance ToQuery DescribeHostedConnections where
        toQuery = const mempty
