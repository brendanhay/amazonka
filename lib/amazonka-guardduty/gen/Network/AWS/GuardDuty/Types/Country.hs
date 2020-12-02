{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Country
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Country where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the country where the remote IP address is located.
--
--
--
-- /See:/ 'country' smart constructor.
data Country = Country'
  { _cCountryName :: !(Maybe Text),
    _cCountryCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Country' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCountryName' - The country name of the remote IP address.
--
-- * 'cCountryCode' - The country code of the remote IP address.
country ::
  Country
country =
  Country' {_cCountryName = Nothing, _cCountryCode = Nothing}

-- | The country name of the remote IP address.
cCountryName :: Lens' Country (Maybe Text)
cCountryName = lens _cCountryName (\s a -> s {_cCountryName = a})

-- | The country code of the remote IP address.
cCountryCode :: Lens' Country (Maybe Text)
cCountryCode = lens _cCountryCode (\s a -> s {_cCountryCode = a})

instance FromJSON Country where
  parseJSON =
    withObject
      "Country"
      ( \x ->
          Country' <$> (x .:? "countryName") <*> (x .:? "countryCode")
      )

instance Hashable Country

instance NFData Country
