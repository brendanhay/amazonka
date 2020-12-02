{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneMessage where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a message about an Availability Zone, Local Zone, or Wavelength Zone.
--
--
--
-- /See:/ 'availabilityZoneMessage' smart constructor.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage'
  { _azmMessage ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZoneMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azmMessage' - The message about the Availability Zone, Local Zone, or Wavelength Zone.
availabilityZoneMessage ::
  AvailabilityZoneMessage
availabilityZoneMessage =
  AvailabilityZoneMessage' {_azmMessage = Nothing}

-- | The message about the Availability Zone, Local Zone, or Wavelength Zone.
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage = lens _azmMessage (\s a -> s {_azmMessage = a})

instance FromXML AvailabilityZoneMessage where
  parseXML x = AvailabilityZoneMessage' <$> (x .@? "message")

instance Hashable AvailabilityZoneMessage

instance NFData AvailabilityZoneMessage
