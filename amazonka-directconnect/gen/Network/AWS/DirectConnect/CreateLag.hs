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
-- Module      : Network.AWS.DirectConnect.CreateLag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new link aggregation group (LAG) with the specified number of bundled physical connections between the customer network and a specific AWS Direct Connect location. A LAG is a logical interface that uses the Link Aggregation Control Protocol (LACP) to aggregate multiple 1 gigabit or 10 gigabit interfaces, allowing you to treat them as a single interface.
--
--
-- All connections in a LAG must use the same bandwidth (for example, 10 Gbps), and must terminate at the same AWS Direct Connect endpoint.
--
-- You can have up to 10 connections per LAG. Regardless of this limit, if you request more connections for the LAG than AWS Direct Connect can allocate on a single endpoint, no LAG is created.
--
-- You can specify an existing physical connection or interconnect to include in the LAG (which counts towards the total number of connections). Doing so interrupts the current physical connection or hosted connections, and re-establishes them as a member of the LAG. The LAG will be created on the same AWS Direct Connect endpoint to which the connection terminates. Any virtual interfaces associated with the connection are automatically disassociated and re-associated with the LAG. The connection ID does not change.
--
-- If the AWS account used to create a LAG is a registered AWS Direct Connect partner, the LAG is automatically enabled to host sub-connections. For a LAG owned by a partner, any associated virtual interfaces cannot be directly configured.
--
module Network.AWS.DirectConnect.CreateLag
    (
    -- * Creating a Request
      createLag
    , CreateLag
    -- * Request Lenses
    , clConnectionId
    , clNumberOfConnections
    , clLocation
    , clConnectionsBandwidth
    , clLagName

    -- * Destructuring the Response
    , lag
    , Lag
    -- * Response Lenses
    , lagLagId
    , lagConnectionsBandwidth
    , lagMinimumLinks
    , lagLagName
    , lagLocation
    , lagConnections
    , lagAwsDevice
    , lagAllowsHostedConnections
    , lagNumberOfConnections
    , lagLagState
    , lagOwnerAccount
    , lagRegion
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the CreateLag operation.
--
--
--
-- /See:/ 'createLag' smart constructor.
data CreateLag = CreateLag'
  { _clConnectionId         :: !(Maybe Text)
  , _clNumberOfConnections  :: !Int
  , _clLocation             :: !Text
  , _clConnectionsBandwidth :: !Text
  , _clLagName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clConnectionId' - The ID of an existing connection to migrate to the LAG. Default: None
--
-- * 'clNumberOfConnections' - The number of physical connections initially provisioned and bundled by the LAG. Default: None
--
-- * 'clLocation' - The AWS Direct Connect location in which the LAG should be allocated. Example: EqSV5 Default: None
--
-- * 'clConnectionsBandwidth' - The bandwidth of the individual physical connections bundled by the LAG. Default: None Available values: 1Gbps, 10Gbps
--
-- * 'clLagName' - The name of the LAG. Example: "@3x10G LAG to AWS@ " Default: None
createLag
    :: Int -- ^ 'clNumberOfConnections'
    -> Text -- ^ 'clLocation'
    -> Text -- ^ 'clConnectionsBandwidth'
    -> Text -- ^ 'clLagName'
    -> CreateLag
createLag pNumberOfConnections_ pLocation_ pConnectionsBandwidth_ pLagName_ =
  CreateLag'
    { _clConnectionId = Nothing
    , _clNumberOfConnections = pNumberOfConnections_
    , _clLocation = pLocation_
    , _clConnectionsBandwidth = pConnectionsBandwidth_
    , _clLagName = pLagName_
    }


-- | The ID of an existing connection to migrate to the LAG. Default: None
clConnectionId :: Lens' CreateLag (Maybe Text)
clConnectionId = lens _clConnectionId (\ s a -> s{_clConnectionId = a})

-- | The number of physical connections initially provisioned and bundled by the LAG. Default: None
clNumberOfConnections :: Lens' CreateLag Int
clNumberOfConnections = lens _clNumberOfConnections (\ s a -> s{_clNumberOfConnections = a})

-- | The AWS Direct Connect location in which the LAG should be allocated. Example: EqSV5 Default: None
clLocation :: Lens' CreateLag Text
clLocation = lens _clLocation (\ s a -> s{_clLocation = a})

-- | The bandwidth of the individual physical connections bundled by the LAG. Default: None Available values: 1Gbps, 10Gbps
clConnectionsBandwidth :: Lens' CreateLag Text
clConnectionsBandwidth = lens _clConnectionsBandwidth (\ s a -> s{_clConnectionsBandwidth = a})

-- | The name of the LAG. Example: "@3x10G LAG to AWS@ " Default: None
clLagName :: Lens' CreateLag Text
clLagName = lens _clLagName (\ s a -> s{_clLagName = a})

instance AWSRequest CreateLag where
        type Rs CreateLag = Lag
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateLag where

instance NFData CreateLag where

instance ToHeaders CreateLag where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreateLag" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLag where
        toJSON CreateLag'{..}
          = object
              (catMaybes
                 [("connectionId" .=) <$> _clConnectionId,
                  Just
                    ("numberOfConnections" .= _clNumberOfConnections),
                  Just ("location" .= _clLocation),
                  Just
                    ("connectionsBandwidth" .= _clConnectionsBandwidth),
                  Just ("lagName" .= _clLagName)])

instance ToPath CreateLag where
        toPath = const "/"

instance ToQuery CreateLag where
        toQuery = const mempty
