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
-- Module      : Network.AWS.RDS.CreateCustomAvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom Availability Zone (AZ).
--
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
module Network.AWS.RDS.CreateCustomAvailabilityZone
  ( -- * Creating a Request
    createCustomAvailabilityZone,
    CreateCustomAvailabilityZone,

    -- * Request Lenses
    ccazVPNTunnelOriginatorIP,
    ccazNewVPNTunnelName,
    ccazExistingVPNId,
    ccazCustomAvailabilityZoneName,

    -- * Destructuring the Response
    createCustomAvailabilityZoneResponse,
    CreateCustomAvailabilityZoneResponse,

    -- * Response Lenses
    ccazrsCustomAvailabilityZone,
    ccazrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createCustomAvailabilityZone' smart constructor.
data CreateCustomAvailabilityZone = CreateCustomAvailabilityZone'
  { _ccazVPNTunnelOriginatorIP ::
      !(Maybe Text),
    _ccazNewVPNTunnelName ::
      !(Maybe Text),
    _ccazExistingVPNId ::
      !(Maybe Text),
    _ccazCustomAvailabilityZoneName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCustomAvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccazVPNTunnelOriginatorIP' - The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic. Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- * 'ccazNewVPNTunnelName' - The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster. Specify this parameter only if @ExistingVpnId@ isn't specified.
--
-- * 'ccazExistingVPNId' - The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
--
-- * 'ccazCustomAvailabilityZoneName' - The name of the custom Availability Zone (AZ).
createCustomAvailabilityZone ::
  -- | 'ccazCustomAvailabilityZoneName'
  Text ->
  CreateCustomAvailabilityZone
createCustomAvailabilityZone pCustomAvailabilityZoneName_ =
  CreateCustomAvailabilityZone'
    { _ccazVPNTunnelOriginatorIP =
        Nothing,
      _ccazNewVPNTunnelName = Nothing,
      _ccazExistingVPNId = Nothing,
      _ccazCustomAvailabilityZoneName = pCustomAvailabilityZoneName_
    }

-- | The IP address of network traffic from your on-premises data center. A custom AZ receives the network traffic. Specify this parameter only if @ExistingVpnId@ isn't specified.
ccazVPNTunnelOriginatorIP :: Lens' CreateCustomAvailabilityZone (Maybe Text)
ccazVPNTunnelOriginatorIP = lens _ccazVPNTunnelOriginatorIP (\s a -> s {_ccazVPNTunnelOriginatorIP = a})

-- | The name of a new VPN tunnel between the Amazon RDS website and the VMware vSphere cluster. Specify this parameter only if @ExistingVpnId@ isn't specified.
ccazNewVPNTunnelName :: Lens' CreateCustomAvailabilityZone (Maybe Text)
ccazNewVPNTunnelName = lens _ccazNewVPNTunnelName (\s a -> s {_ccazNewVPNTunnelName = a})

-- | The ID of an existing virtual private network (VPN) between the Amazon RDS website and the VMware vSphere cluster.
ccazExistingVPNId :: Lens' CreateCustomAvailabilityZone (Maybe Text)
ccazExistingVPNId = lens _ccazExistingVPNId (\s a -> s {_ccazExistingVPNId = a})

-- | The name of the custom Availability Zone (AZ).
ccazCustomAvailabilityZoneName :: Lens' CreateCustomAvailabilityZone Text
ccazCustomAvailabilityZoneName = lens _ccazCustomAvailabilityZoneName (\s a -> s {_ccazCustomAvailabilityZoneName = a})

instance AWSRequest CreateCustomAvailabilityZone where
  type
    Rs CreateCustomAvailabilityZone =
      CreateCustomAvailabilityZoneResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "CreateCustomAvailabilityZoneResult"
      ( \s h x ->
          CreateCustomAvailabilityZoneResponse'
            <$> (x .@? "CustomAvailabilityZone") <*> (pure (fromEnum s))
      )

instance Hashable CreateCustomAvailabilityZone

instance NFData CreateCustomAvailabilityZone

instance ToHeaders CreateCustomAvailabilityZone where
  toHeaders = const mempty

instance ToPath CreateCustomAvailabilityZone where
  toPath = const "/"

instance ToQuery CreateCustomAvailabilityZone where
  toQuery CreateCustomAvailabilityZone' {..} =
    mconcat
      [ "Action" =: ("CreateCustomAvailabilityZone" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "VpnTunnelOriginatorIP" =: _ccazVPNTunnelOriginatorIP,
        "NewVpnTunnelName" =: _ccazNewVPNTunnelName,
        "ExistingVpnId" =: _ccazExistingVPNId,
        "CustomAvailabilityZoneName" =: _ccazCustomAvailabilityZoneName
      ]

-- | /See:/ 'createCustomAvailabilityZoneResponse' smart constructor.
data CreateCustomAvailabilityZoneResponse = CreateCustomAvailabilityZoneResponse'
  { _ccazrsCustomAvailabilityZone ::
      !( Maybe
           CustomAvailabilityZone
       ),
    _ccazrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCustomAvailabilityZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccazrsCustomAvailabilityZone' - Undocumented member.
--
-- * 'ccazrsResponseStatus' - -- | The response status code.
createCustomAvailabilityZoneResponse ::
  -- | 'ccazrsResponseStatus'
  Int ->
  CreateCustomAvailabilityZoneResponse
createCustomAvailabilityZoneResponse pResponseStatus_ =
  CreateCustomAvailabilityZoneResponse'
    { _ccazrsCustomAvailabilityZone =
        Nothing,
      _ccazrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ccazrsCustomAvailabilityZone :: Lens' CreateCustomAvailabilityZoneResponse (Maybe CustomAvailabilityZone)
ccazrsCustomAvailabilityZone = lens _ccazrsCustomAvailabilityZone (\s a -> s {_ccazrsCustomAvailabilityZone = a})

-- | -- | The response status code.
ccazrsResponseStatus :: Lens' CreateCustomAvailabilityZoneResponse Int
ccazrsResponseStatus = lens _ccazrsResponseStatus (\s a -> s {_ccazrsResponseStatus = a})

instance NFData CreateCustomAvailabilityZoneResponse
