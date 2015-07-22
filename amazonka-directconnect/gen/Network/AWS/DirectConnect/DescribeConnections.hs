{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Displays all connections in this region.
--
-- If a connection ID is provided, the call returns only that particular
-- connection.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnections.html>
module Network.AWS.DirectConnect.DescribeConnections
    (
    -- * Request
      DescribeConnections
    -- ** Request constructor
    , describeConnections
    -- ** Request lenses
    , drqConnectionId

    -- * Response
    , Connections
    -- ** Response constructor
    , connections
    -- ** Response lenses
    , cConnections
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DescribeConnections operation.
--
-- /See:/ 'describeConnections' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqConnectionId'
newtype DescribeConnections = DescribeConnections'
    { _drqConnectionId :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConnections' smart constructor.
describeConnections :: DescribeConnections
describeConnections =
    DescribeConnections'
    { _drqConnectionId = Nothing
    }

-- | FIXME: Undocumented member.
drqConnectionId :: Lens' DescribeConnections (Maybe Text)
drqConnectionId = lens _drqConnectionId (\ s a -> s{_drqConnectionId = a});

instance AWSRequest DescribeConnections where
        type Sv DescribeConnections = DirectConnect
        type Rs DescribeConnections = Connections
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

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
          = object ["connectionId" .= _drqConnectionId]

instance ToPath DescribeConnections where
        toPath = const "/"

instance ToQuery DescribeConnections where
        toQuery = const mempty
