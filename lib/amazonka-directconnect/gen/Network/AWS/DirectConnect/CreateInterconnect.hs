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
-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an interconnect between an AWS Direct Connect Partner's network and a specific AWS Direct Connect location.
--
--
-- An interconnect is a connection that is capable of hosting other connections. The AWS Direct Connect partner can use an interconnect to provide AWS Direct Connect hosted connections to customers through their own network services. Like a standard connection, an interconnect links the partner's network to an AWS Direct Connect location over a standard Ethernet fiber-optic cable. One end is connected to the partner's router, the other to an AWS Direct Connect router.
--
-- You can automatically add the new interconnect to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new interconnect is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no interconnect is created.
--
-- For each end customer, the AWS Direct Connect Partner provisions a connection on their interconnect by calling 'AllocateHostedConnection' . The end customer can then connect to AWS resources by creating a virtual interface on their connection, using the VLAN assigned to them by the AWS Direct Connect Partner.
module Network.AWS.DirectConnect.CreateInterconnect
  ( -- * Creating a Request
    createInterconnect,
    CreateInterconnect,

    -- * Request Lenses
    ciLagId,
    ciProviderName,
    ciTags,
    ciInterconnectName,
    ciBandwidth,
    ciLocation,

    -- * Destructuring the Response
    interconnect,
    Interconnect,

    -- * Response Lenses
    iLagId,
    iInterconnectId,
    iLocation,
    iInterconnectName,
    iAwsDevice,
    iHasLogicalRedundancy,
    iLoaIssueTime,
    iBandwidth,
    iJumboFrameCapable,
    iInterconnectState,
    iRegion,
    iProviderName,
    iAwsDeviceV2,
    iTags,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInterconnect' smart constructor.
data CreateInterconnect = CreateInterconnect'
  { _ciLagId ::
      !(Maybe Text),
    _ciProviderName :: !(Maybe Text),
    _ciTags :: !(Maybe (List1 Tag)),
    _ciInterconnectName :: !Text,
    _ciBandwidth :: !Text,
    _ciLocation :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInterconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciLagId' - The ID of the LAG.
--
-- * 'ciProviderName' - The name of the service provider associated with the interconnect.
--
-- * 'ciTags' - The tags to associate with the interconnect.
--
-- * 'ciInterconnectName' - The name of the interconnect.
--
-- * 'ciBandwidth' - The port bandwidth, in Gbps. The possible values are 1 and 10.
--
-- * 'ciLocation' - The location of the interconnect.
createInterconnect ::
  -- | 'ciInterconnectName'
  Text ->
  -- | 'ciBandwidth'
  Text ->
  -- | 'ciLocation'
  Text ->
  CreateInterconnect
createInterconnect pInterconnectName_ pBandwidth_ pLocation_ =
  CreateInterconnect'
    { _ciLagId = Nothing,
      _ciProviderName = Nothing,
      _ciTags = Nothing,
      _ciInterconnectName = pInterconnectName_,
      _ciBandwidth = pBandwidth_,
      _ciLocation = pLocation_
    }

-- | The ID of the LAG.
ciLagId :: Lens' CreateInterconnect (Maybe Text)
ciLagId = lens _ciLagId (\s a -> s {_ciLagId = a})

-- | The name of the service provider associated with the interconnect.
ciProviderName :: Lens' CreateInterconnect (Maybe Text)
ciProviderName = lens _ciProviderName (\s a -> s {_ciProviderName = a})

-- | The tags to associate with the interconnect.
ciTags :: Lens' CreateInterconnect (Maybe (NonEmpty Tag))
ciTags = lens _ciTags (\s a -> s {_ciTags = a}) . mapping _List1

-- | The name of the interconnect.
ciInterconnectName :: Lens' CreateInterconnect Text
ciInterconnectName = lens _ciInterconnectName (\s a -> s {_ciInterconnectName = a})

-- | The port bandwidth, in Gbps. The possible values are 1 and 10.
ciBandwidth :: Lens' CreateInterconnect Text
ciBandwidth = lens _ciBandwidth (\s a -> s {_ciBandwidth = a})

-- | The location of the interconnect.
ciLocation :: Lens' CreateInterconnect Text
ciLocation = lens _ciLocation (\s a -> s {_ciLocation = a})

instance AWSRequest CreateInterconnect where
  type Rs CreateInterconnect = Interconnect
  request = postJSON directConnect
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable CreateInterconnect

instance NFData CreateInterconnect

instance ToHeaders CreateInterconnect where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.CreateInterconnect" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateInterconnect where
  toJSON CreateInterconnect' {..} =
    object
      ( catMaybes
          [ ("lagId" .=) <$> _ciLagId,
            ("providerName" .=) <$> _ciProviderName,
            ("tags" .=) <$> _ciTags,
            Just ("interconnectName" .= _ciInterconnectName),
            Just ("bandwidth" .= _ciBandwidth),
            Just ("location" .= _ciLocation)
          ]
      )

instance ToPath CreateInterconnect where
  toPath = const "/"

instance ToQuery CreateInterconnect where
  toQuery = const mempty
