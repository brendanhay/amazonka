{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateDNSDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateDNSDetails where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the Private DNS name for interface endpoints.
--
--
--
-- /See:/ 'privateDNSDetails' smart constructor.
newtype PrivateDNSDetails = PrivateDNSDetails'
  { _pddPrivateDNSName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrivateDNSDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pddPrivateDNSName' - The private DNS name assigned to the VPC endpoint service.
privateDNSDetails ::
  PrivateDNSDetails
privateDNSDetails =
  PrivateDNSDetails' {_pddPrivateDNSName = Nothing}

-- | The private DNS name assigned to the VPC endpoint service.
pddPrivateDNSName :: Lens' PrivateDNSDetails (Maybe Text)
pddPrivateDNSName = lens _pddPrivateDNSName (\s a -> s {_pddPrivateDNSName = a})

instance FromXML PrivateDNSDetails where
  parseXML x = PrivateDNSDetails' <$> (x .@? "privateDnsName")

instance Hashable PrivateDNSDetails

instance NFData PrivateDNSDetails
