{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AvailabilityZone where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SupportedPlatform

-- | Describes an availability zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azName :: !(Maybe Text),
    _azSupportedPlatforms :: !(Maybe [SupportedPlatform])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the availability zone.
--
-- * 'azSupportedPlatforms' -
availabilityZone ::
  AvailabilityZone
availabilityZone =
  AvailabilityZone'
    { _azName = Nothing,
      _azSupportedPlatforms = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s {_azName = a})

-- |
azSupportedPlatforms :: Lens' AvailabilityZone [SupportedPlatform]
azSupportedPlatforms = lens _azSupportedPlatforms (\s a -> s {_azSupportedPlatforms = a}) . _Default . _Coerce

instance FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      <$> (x .@? "Name")
      <*> ( x .@? "SupportedPlatforms" .!@ mempty
              >>= may (parseXMLList "SupportedPlatform")
          )

instance Hashable AvailabilityZone

instance NFData AvailabilityZone
