{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Organization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Organization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the ISP organization of the remote IP address.
--
--
--
-- /See:/ 'organization' smart constructor.
data Organization = Organization'
  { _oOrg :: !(Maybe Text),
    _oASNOrg :: !(Maybe Text),
    _oASN :: !(Maybe Text),
    _oIsp :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Organization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOrg' - The name of the internet provider.
--
-- * 'oASNOrg' - The organization that registered this ASN.
--
-- * 'oASN' - The Autonomous System Number (ASN) of the internet provider of the remote IP address.
--
-- * 'oIsp' - The ISP information for the internet provider.
organization ::
  Organization
organization =
  Organization'
    { _oOrg = Nothing,
      _oASNOrg = Nothing,
      _oASN = Nothing,
      _oIsp = Nothing
    }

-- | The name of the internet provider.
oOrg :: Lens' Organization (Maybe Text)
oOrg = lens _oOrg (\s a -> s {_oOrg = a})

-- | The organization that registered this ASN.
oASNOrg :: Lens' Organization (Maybe Text)
oASNOrg = lens _oASNOrg (\s a -> s {_oASNOrg = a})

-- | The Autonomous System Number (ASN) of the internet provider of the remote IP address.
oASN :: Lens' Organization (Maybe Text)
oASN = lens _oASN (\s a -> s {_oASN = a})

-- | The ISP information for the internet provider.
oIsp :: Lens' Organization (Maybe Text)
oIsp = lens _oIsp (\s a -> s {_oIsp = a})

instance FromJSON Organization where
  parseJSON =
    withObject
      "Organization"
      ( \x ->
          Organization'
            <$> (x .:? "org")
            <*> (x .:? "asnOrg")
            <*> (x .:? "asn")
            <*> (x .:? "isp")
      )

instance Hashable Organization

instance NFData Organization
