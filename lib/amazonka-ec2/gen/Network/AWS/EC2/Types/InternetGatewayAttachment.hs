{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InternetGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InternetGatewayAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the attachment of a VPC to an internet gateway or an egress-only internet gateway.
--
--
--
-- /See:/ 'internetGatewayAttachment' smart constructor.
data InternetGatewayAttachment = InternetGatewayAttachment'
  { _igaState ::
      !AttachmentStatus,
    _igaVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InternetGatewayAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igaState' - The current state of the attachment. For an internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
--
-- * 'igaVPCId' - The ID of the VPC.
internetGatewayAttachment ::
  -- | 'igaState'
  AttachmentStatus ->
  -- | 'igaVPCId'
  Text ->
  InternetGatewayAttachment
internetGatewayAttachment pState_ pVPCId_ =
  InternetGatewayAttachment'
    { _igaState = pState_,
      _igaVPCId = pVPCId_
    }

-- | The current state of the attachment. For an internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
igaState :: Lens' InternetGatewayAttachment AttachmentStatus
igaState = lens _igaState (\s a -> s {_igaState = a})

-- | The ID of the VPC.
igaVPCId :: Lens' InternetGatewayAttachment Text
igaVPCId = lens _igaVPCId (\s a -> s {_igaVPCId = a})

instance FromXML InternetGatewayAttachment where
  parseXML x =
    InternetGatewayAttachment' <$> (x .@ "state") <*> (x .@ "vpcId")

instance Hashable InternetGatewayAttachment

instance NFData InternetGatewayAttachment
