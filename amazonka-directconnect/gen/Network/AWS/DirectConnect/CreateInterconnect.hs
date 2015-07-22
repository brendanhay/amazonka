{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new interconnect between a AWS Direct Connect partner\'s
-- network and a specific AWS Direct Connect location.
--
-- An interconnect is a connection which is capable of hosting other
-- connections. The AWS Direct Connect partner can use an interconnect to
-- provide sub-1Gbps AWS Direct Connect service to tier 2 customers who do
-- not have their own connections. Like a standard connection, an
-- interconnect links the AWS Direct Connect partner\'s network to an AWS
-- Direct Connect location over a standard 1 Gbps or 10 Gbps Ethernet
-- fiber-optic cable. One end is connected to the partner\'s router, the
-- other to an AWS Direct Connect router.
--
-- For each end customer, the AWS Direct Connect partner provisions a
-- connection on their interconnect by calling
-- AllocateConnectionOnInterconnect. The end customer can then connect to
-- AWS resources by creating a virtual interface on their connection, using
-- the VLAN assigned to them by the AWS Direct Connect partner.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreateInterconnect.html>
module Network.AWS.DirectConnect.CreateInterconnect
    (
    -- * Request
      CreateInterconnect
    -- ** Request constructor
    , createInterconnect
    -- ** Request lenses
    , cirqInterconnectName
    , cirqBandwidth
    , cirqLocation

    -- * Response
    , Interconnect
    -- ** Response constructor
    , interconnect
    -- ** Response lenses
    , iInterconnectId
    , iInterconnectName
    , iLocation
    , iBandwidth
    , iInterconnectState
    , iRegion
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the CreateInterconnect operation.
--
-- /See:/ 'createInterconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirqInterconnectName'
--
-- * 'cirqBandwidth'
--
-- * 'cirqLocation'
data CreateInterconnect = CreateInterconnect'
    { _cirqInterconnectName :: !Text
    , _cirqBandwidth        :: !Text
    , _cirqLocation         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInterconnect' smart constructor.
createInterconnect :: Text -> Text -> Text -> CreateInterconnect
createInterconnect pInterconnectName pBandwidth pLocation =
    CreateInterconnect'
    { _cirqInterconnectName = pInterconnectName
    , _cirqBandwidth = pBandwidth
    , _cirqLocation = pLocation
    }

-- | The name of the interconnect.
--
-- Example: \"/1G Interconnect to AWS/\"
--
-- Default: None
cirqInterconnectName :: Lens' CreateInterconnect Text
cirqInterconnectName = lens _cirqInterconnectName (\ s a -> s{_cirqInterconnectName = a});

-- | The port bandwidth
--
-- Example: 1Gbps
--
-- Default: None
--
-- Available values: 1Gbps,10Gbps
cirqBandwidth :: Lens' CreateInterconnect Text
cirqBandwidth = lens _cirqBandwidth (\ s a -> s{_cirqBandwidth = a});

-- | Where the interconnect is located
--
-- Example: EqSV5
--
-- Default: None
cirqLocation :: Lens' CreateInterconnect Text
cirqLocation = lens _cirqLocation (\ s a -> s{_cirqLocation = a});

instance AWSRequest CreateInterconnect where
        type Sv CreateInterconnect = DirectConnect
        type Rs CreateInterconnect = Interconnect
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateInterconnect where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreateInterconnect" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateInterconnect where
        toJSON CreateInterconnect'{..}
          = object
              ["interconnectName" .= _cirqInterconnectName,
               "bandwidth" .= _cirqBandwidth,
               "location" .= _cirqLocation]

instance ToPath CreateInterconnect where
        toPath = const "/"

instance ToQuery CreateInterconnect where
        toQuery = const mempty
