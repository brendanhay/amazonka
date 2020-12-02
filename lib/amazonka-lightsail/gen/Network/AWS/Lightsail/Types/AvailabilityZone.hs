{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AvailabilityZone where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azState ::
      !(Maybe Text),
    _azZoneName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azState' - The state of the Availability Zone.
--
-- * 'azZoneName' - The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
availabilityZone ::
  AvailabilityZone
availabilityZone =
  AvailabilityZone' {_azState = Nothing, _azZoneName = Nothing}

-- | The state of the Availability Zone.
azState :: Lens' AvailabilityZone (Maybe Text)
azState = lens _azState (\s a -> s {_azState = a})

-- | The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\s a -> s {_azZoneName = a})

instance FromJSON AvailabilityZone where
  parseJSON =
    withObject
      "AvailabilityZone"
      ( \x ->
          AvailabilityZone' <$> (x .:? "state") <*> (x .:? "zoneName")
      )

instance Hashable AvailabilityZone

instance NFData AvailabilityZone
