{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.City
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.City where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the city associated with the IP address.
--
--
--
-- /See:/ 'city' smart constructor.
newtype City = City' {_cCityName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'City' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCityName' - The city name of the remote IP address.
city ::
  City
city = City' {_cCityName = Nothing}

-- | The city name of the remote IP address.
cCityName :: Lens' City (Maybe Text)
cCityName = lens _cCityName (\s a -> s {_cCityName = a})

instance FromJSON City where
  parseJSON = withObject "City" (\x -> City' <$> (x .:? "cityName"))

instance Hashable City

instance NFData City
