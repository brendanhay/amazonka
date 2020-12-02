{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DNSRequestAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DNSRequestAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the DNS_REQUEST action described in this finding.
--
--
--
-- /See:/ 'dnsRequestAction' smart constructor.
newtype DNSRequestAction = DNSRequestAction'
  { _draDomain ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DNSRequestAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'draDomain' - The domain information for the API request.
dnsRequestAction ::
  DNSRequestAction
dnsRequestAction = DNSRequestAction' {_draDomain = Nothing}

-- | The domain information for the API request.
draDomain :: Lens' DNSRequestAction (Maybe Text)
draDomain = lens _draDomain (\s a -> s {_draDomain = a})

instance FromJSON DNSRequestAction where
  parseJSON =
    withObject
      "DNSRequestAction"
      (\x -> DNSRequestAction' <$> (x .:? "domain"))

instance Hashable DNSRequestAction

instance NFData DNSRequestAction
