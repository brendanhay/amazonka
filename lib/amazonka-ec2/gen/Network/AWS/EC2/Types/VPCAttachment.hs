{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an attachment between a virtual private gateway and a VPC.
--
--
--
-- /See:/ 'vpcAttachment' smart constructor.
data VPCAttachment = VPCAttachment'
  { _vaState ::
      !(Maybe AttachmentStatus),
    _vaVPCId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vaState' - The current state of the attachment.
--
-- * 'vaVPCId' - The ID of the VPC.
vpcAttachment ::
  VPCAttachment
vpcAttachment =
  VPCAttachment' {_vaState = Nothing, _vaVPCId = Nothing}

-- | The current state of the attachment.
vaState :: Lens' VPCAttachment (Maybe AttachmentStatus)
vaState = lens _vaState (\s a -> s {_vaState = a})

-- | The ID of the VPC.
vaVPCId :: Lens' VPCAttachment (Maybe Text)
vaVPCId = lens _vaVPCId (\s a -> s {_vaVPCId = a})

instance FromXML VPCAttachment where
  parseXML x = VPCAttachment' <$> (x .@? "state") <*> (x .@? "vpcId")

instance Hashable VPCAttachment

instance NFData VPCAttachment
