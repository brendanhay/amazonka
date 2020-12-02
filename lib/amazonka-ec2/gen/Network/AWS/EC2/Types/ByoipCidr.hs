{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ByoipCidr where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ByoipCidrState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an address range that is provisioned for use with your AWS resources through bring your own IP addresses (BYOIP).
--
--
--
-- /See:/ 'byoipCidr' smart constructor.
data ByoipCidr = ByoipCidr'
  { _bcState :: !(Maybe ByoipCidrState),
    _bcCidr :: !(Maybe Text),
    _bcStatusMessage :: !(Maybe Text),
    _bcDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ByoipCidr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcState' - The state of the address pool.
--
-- * 'bcCidr' - The address range, in CIDR notation.
--
-- * 'bcStatusMessage' - Upon success, contains the ID of the address pool. Otherwise, contains an error message.
--
-- * 'bcDescription' - The description of the address range.
byoipCidr ::
  ByoipCidr
byoipCidr =
  ByoipCidr'
    { _bcState = Nothing,
      _bcCidr = Nothing,
      _bcStatusMessage = Nothing,
      _bcDescription = Nothing
    }

-- | The state of the address pool.
bcState :: Lens' ByoipCidr (Maybe ByoipCidrState)
bcState = lens _bcState (\s a -> s {_bcState = a})

-- | The address range, in CIDR notation.
bcCidr :: Lens' ByoipCidr (Maybe Text)
bcCidr = lens _bcCidr (\s a -> s {_bcCidr = a})

-- | Upon success, contains the ID of the address pool. Otherwise, contains an error message.
bcStatusMessage :: Lens' ByoipCidr (Maybe Text)
bcStatusMessage = lens _bcStatusMessage (\s a -> s {_bcStatusMessage = a})

-- | The description of the address range.
bcDescription :: Lens' ByoipCidr (Maybe Text)
bcDescription = lens _bcDescription (\s a -> s {_bcDescription = a})

instance FromXML ByoipCidr where
  parseXML x =
    ByoipCidr'
      <$> (x .@? "state")
      <*> (x .@? "cidr")
      <*> (x .@? "statusMessage")
      <*> (x .@? "description")

instance Hashable ByoipCidr

instance NFData ByoipCidr
