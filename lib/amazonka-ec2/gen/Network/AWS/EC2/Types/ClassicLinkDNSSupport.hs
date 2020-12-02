{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLinkDNSSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLinkDNSSupport where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the ClassicLink DNS support status of a VPC.
--
--
--
-- /See:/ 'classicLinkDNSSupport' smart constructor.
data ClassicLinkDNSSupport = ClassicLinkDNSSupport'
  { _cldsVPCId ::
      !(Maybe Text),
    _cldsClassicLinkDNSSupported :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cldsVPCId' - The ID of the VPC.
--
-- * 'cldsClassicLinkDNSSupported' - Indicates whether ClassicLink DNS support is enabled for the VPC.
classicLinkDNSSupport ::
  ClassicLinkDNSSupport
classicLinkDNSSupport =
  ClassicLinkDNSSupport'
    { _cldsVPCId = Nothing,
      _cldsClassicLinkDNSSupported = Nothing
    }

-- | The ID of the VPC.
cldsVPCId :: Lens' ClassicLinkDNSSupport (Maybe Text)
cldsVPCId = lens _cldsVPCId (\s a -> s {_cldsVPCId = a})

-- | Indicates whether ClassicLink DNS support is enabled for the VPC.
cldsClassicLinkDNSSupported :: Lens' ClassicLinkDNSSupport (Maybe Bool)
cldsClassicLinkDNSSupported = lens _cldsClassicLinkDNSSupported (\s a -> s {_cldsClassicLinkDNSSupported = a})

instance FromXML ClassicLinkDNSSupport where
  parseXML x =
    ClassicLinkDNSSupport'
      <$> (x .@? "vpcId") <*> (x .@? "classicLinkDnsSupported")

instance Hashable ClassicLinkDNSSupport

instance NFData ClassicLinkDNSSupport
