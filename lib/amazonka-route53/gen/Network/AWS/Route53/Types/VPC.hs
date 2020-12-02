{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.VPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.VPC where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.VPCRegion

-- | (Private hosted zones only) A complex type that contains information about an Amazon VPC.
--
--
--
-- /See:/ 'vpc' smart constructor.
data VPC = VPC'
  { _vpcVPCRegion :: !(Maybe VPCRegion),
    _vpcVPCId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcVPCRegion' - (Private hosted zones only) The region that an Amazon VPC was created in.
--
-- * 'vpcVPCId' - Undocumented member.
vpc ::
  VPC
vpc = VPC' {_vpcVPCRegion = Nothing, _vpcVPCId = Nothing}

-- | (Private hosted zones only) The region that an Amazon VPC was created in.
vpcVPCRegion :: Lens' VPC (Maybe VPCRegion)
vpcVPCRegion = lens _vpcVPCRegion (\s a -> s {_vpcVPCRegion = a})

-- | Undocumented member.
vpcVPCId :: Lens' VPC (Maybe Text)
vpcVPCId = lens _vpcVPCId (\s a -> s {_vpcVPCId = a})

instance FromXML VPC where
  parseXML x = VPC' <$> (x .@? "VPCRegion") <*> (x .@? "VPCId")

instance Hashable VPC

instance NFData VPC

instance ToXML VPC where
  toXML VPC' {..} =
    mconcat ["VPCRegion" @= _vpcVPCRegion, "VPCId" @= _vpcVPCId]
