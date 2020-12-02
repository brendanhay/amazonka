{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection between a customer network and a specific AWS Direct Connect location.
--
--
-- A connection links your internal network to an AWS Direct Connect location over a standard Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router.
--
-- To find the locations for your Region, use 'DescribeLocations' .
--
-- You can automatically add the new connection to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new connection is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no connection is created.
module Network.AWS.DirectConnect.CreateConnection
  ( -- * Creating a Request
    createConnection,
    CreateConnection,

    -- * Request Lenses
    ccLagId,
    ccProviderName,
    ccTags,
    ccLocation,
    ccBandwidth,
    ccConnectionName,

    -- * Destructuring the Response
    connection,
    Connection,

    -- * Response Lenses
    cLagId,
    cVlan,
    cLocation,
    cAwsDevice,
    cHasLogicalRedundancy,
    cConnectionId,
    cLoaIssueTime,
    cPartnerName,
    cConnectionName,
    cBandwidth,
    cJumboFrameCapable,
    cOwnerAccount,
    cRegion,
    cProviderName,
    cAwsDeviceV2,
    cConnectionState,
    cTags,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createConnection' smart constructor.
data CreateConnection = CreateConnection'
  { _ccLagId ::
      !(Maybe Text),
    _ccProviderName :: !(Maybe Text),
    _ccTags :: !(Maybe (List1 Tag)),
    _ccLocation :: !Text,
    _ccBandwidth :: !Text,
    _ccConnectionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccLagId' - The ID of the LAG.
--
-- * 'ccProviderName' - The name of the service provider associated with the requested connection.
--
-- * 'ccTags' - The tags to associate with the lag.
--
-- * 'ccLocation' - The location of the connection.
--
-- * 'ccBandwidth' - The bandwidth of the connection.
--
-- * 'ccConnectionName' - The name of the connection.
createConnection ::
  -- | 'ccLocation'
  Text ->
  -- | 'ccBandwidth'
  Text ->
  -- | 'ccConnectionName'
  Text ->
  CreateConnection
createConnection pLocation_ pBandwidth_ pConnectionName_ =
  CreateConnection'
    { _ccLagId = Nothing,
      _ccProviderName = Nothing,
      _ccTags = Nothing,
      _ccLocation = pLocation_,
      _ccBandwidth = pBandwidth_,
      _ccConnectionName = pConnectionName_
    }

-- | The ID of the LAG.
ccLagId :: Lens' CreateConnection (Maybe Text)
ccLagId = lens _ccLagId (\s a -> s {_ccLagId = a})

-- | The name of the service provider associated with the requested connection.
ccProviderName :: Lens' CreateConnection (Maybe Text)
ccProviderName = lens _ccProviderName (\s a -> s {_ccProviderName = a})

-- | The tags to associate with the lag.
ccTags :: Lens' CreateConnection (Maybe (NonEmpty Tag))
ccTags = lens _ccTags (\s a -> s {_ccTags = a}) . mapping _List1

-- | The location of the connection.
ccLocation :: Lens' CreateConnection Text
ccLocation = lens _ccLocation (\s a -> s {_ccLocation = a})

-- | The bandwidth of the connection.
ccBandwidth :: Lens' CreateConnection Text
ccBandwidth = lens _ccBandwidth (\s a -> s {_ccBandwidth = a})

-- | The name of the connection.
ccConnectionName :: Lens' CreateConnection Text
ccConnectionName = lens _ccConnectionName (\s a -> s {_ccConnectionName = a})

instance AWSRequest CreateConnection where
  type Rs CreateConnection = Connection
  request = postJSON directConnect
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable CreateConnection

instance NFData CreateConnection

instance ToHeaders CreateConnection where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.CreateConnection" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    object
      ( catMaybes
          [ ("lagId" .=) <$> _ccLagId,
            ("providerName" .=) <$> _ccProviderName,
            ("tags" .=) <$> _ccTags,
            Just ("location" .= _ccLocation),
            Just ("bandwidth" .= _ccBandwidth),
            Just ("connectionName" .= _ccConnectionName)
          ]
      )

instance ToPath CreateConnection where
  toPath = const "/"

instance ToQuery CreateConnection where
  toQuery = const mempty
