{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewBGPPeer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewBGPPeer where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a new BGP peer.
--
--
--
-- /See:/ 'newBGPPeer' smart constructor.
data NewBGPPeer = NewBGPPeer'
  { _nbpCustomerAddress :: !(Maybe Text),
    _nbpAmazonAddress :: !(Maybe Text),
    _nbpAddressFamily :: !(Maybe AddressFamily),
    _nbpAsn :: !(Maybe Int),
    _nbpAuthKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewBGPPeer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nbpCustomerAddress' - The IP address assigned to the customer interface.
--
-- * 'nbpAmazonAddress' - The IP address assigned to the Amazon interface.
--
-- * 'nbpAddressFamily' - The address family for the BGP peer.
--
-- * 'nbpAsn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- * 'nbpAuthKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
newBGPPeer ::
  NewBGPPeer
newBGPPeer =
  NewBGPPeer'
    { _nbpCustomerAddress = Nothing,
      _nbpAmazonAddress = Nothing,
      _nbpAddressFamily = Nothing,
      _nbpAsn = Nothing,
      _nbpAuthKey = Nothing
    }

-- | The IP address assigned to the customer interface.
nbpCustomerAddress :: Lens' NewBGPPeer (Maybe Text)
nbpCustomerAddress = lens _nbpCustomerAddress (\s a -> s {_nbpCustomerAddress = a})

-- | The IP address assigned to the Amazon interface.
nbpAmazonAddress :: Lens' NewBGPPeer (Maybe Text)
nbpAmazonAddress = lens _nbpAmazonAddress (\s a -> s {_nbpAmazonAddress = a})

-- | The address family for the BGP peer.
nbpAddressFamily :: Lens' NewBGPPeer (Maybe AddressFamily)
nbpAddressFamily = lens _nbpAddressFamily (\s a -> s {_nbpAddressFamily = a})

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
nbpAsn :: Lens' NewBGPPeer (Maybe Int)
nbpAsn = lens _nbpAsn (\s a -> s {_nbpAsn = a})

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
nbpAuthKey :: Lens' NewBGPPeer (Maybe Text)
nbpAuthKey = lens _nbpAuthKey (\s a -> s {_nbpAuthKey = a})

instance Hashable NewBGPPeer

instance NFData NewBGPPeer

instance ToJSON NewBGPPeer where
  toJSON NewBGPPeer' {..} =
    object
      ( catMaybes
          [ ("customerAddress" .=) <$> _nbpCustomerAddress,
            ("amazonAddress" .=) <$> _nbpAmazonAddress,
            ("addressFamily" .=) <$> _nbpAddressFamily,
            ("asn" .=) <$> _nbpAsn,
            ("authKey" .=) <$> _nbpAuthKey
          ]
      )
