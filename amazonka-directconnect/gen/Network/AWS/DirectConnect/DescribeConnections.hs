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
-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays all connections in this region.
--
--
-- If a connection ID is provided, the call returns only that particular connection.
--
module Network.AWS.DirectConnect.DescribeConnections
    (
    -- * Creating a Request
      describeConnections
    , DescribeConnections
    -- * Request Lenses
    , dConnectionId

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

-- | Container for the parameters to the DescribeConnections operation.
--
--
--
-- /See:/ 'describeConnections' smart constructor.
newtype DescribeConnections = DescribeConnections'
  { _dConnectionId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dConnectionId' - Undocumented member.
describeConnections
    :: DescribeConnections
describeConnections = DescribeConnections' {_dConnectionId = Nothing}


-- | Undocumented member.
dConnectionId :: Lens' DescribeConnections (Maybe Text)
dConnectionId = lens _dConnectionId (\ s a -> s{_dConnectionId = a})

instance AWSRequest DescribeConnections where
        type Rs DescribeConnections = Connections
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeConnections where

instance NFData DescribeConnections where

instance ToHeaders DescribeConnections where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeConnections" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConnections where
        toJSON DescribeConnections'{..}
          = object
              (catMaybes [("connectionId" .=) <$> _dConnectionId])

instance ToPath DescribeConnections where
        toPath = const "/"

instance ToQuery DescribeConnections where
        toQuery = const mempty
