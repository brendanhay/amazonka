{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.LocalIPDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.LocalIPDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the local IP address of the connection.
--
--
--
-- /See:/ 'localIPDetails' smart constructor.
newtype LocalIPDetails = LocalIPDetails'
  { _lidIPAddressV4 ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalIPDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lidIPAddressV4' - The IPv4 local address of the connection.
localIPDetails ::
  LocalIPDetails
localIPDetails = LocalIPDetails' {_lidIPAddressV4 = Nothing}

-- | The IPv4 local address of the connection.
lidIPAddressV4 :: Lens' LocalIPDetails (Maybe Text)
lidIPAddressV4 = lens _lidIPAddressV4 (\s a -> s {_lidIPAddressV4 = a})

instance FromJSON LocalIPDetails where
  parseJSON =
    withObject
      "LocalIPDetails"
      (\x -> LocalIPDetails' <$> (x .:? "ipAddressV4"))

instance Hashable LocalIPDetails

instance NFData LocalIPDetails
