{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionStateReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionStateReason where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VPCPeeringConnectionStateReasonCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of a VPC peering connection.
--
--
--
-- /See:/ 'vpcPeeringConnectionStateReason' smart constructor.
data VPCPeeringConnectionStateReason = VPCPeeringConnectionStateReason'
  { _vpcsrCode ::
      !( Maybe
           VPCPeeringConnectionStateReasonCode
       ),
    _vpcsrMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCPeeringConnectionStateReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcsrCode' - The status of the VPC peering connection.
--
-- * 'vpcsrMessage' - A message that provides more information about the status, if applicable.
vpcPeeringConnectionStateReason ::
  VPCPeeringConnectionStateReason
vpcPeeringConnectionStateReason =
  VPCPeeringConnectionStateReason'
    { _vpcsrCode = Nothing,
      _vpcsrMessage = Nothing
    }

-- | The status of the VPC peering connection.
vpcsrCode :: Lens' VPCPeeringConnectionStateReason (Maybe VPCPeeringConnectionStateReasonCode)
vpcsrCode = lens _vpcsrCode (\s a -> s {_vpcsrCode = a})

-- | A message that provides more information about the status, if applicable.
vpcsrMessage :: Lens' VPCPeeringConnectionStateReason (Maybe Text)
vpcsrMessage = lens _vpcsrMessage (\s a -> s {_vpcsrMessage = a})

instance FromXML VPCPeeringConnectionStateReason where
  parseXML x =
    VPCPeeringConnectionStateReason'
      <$> (x .@? "code") <*> (x .@? "message")

instance Hashable VPCPeeringConnectionStateReason

instance NFData VPCPeeringConnectionStateReason
