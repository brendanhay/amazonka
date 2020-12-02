{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AvailabilityZoneDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AvailabilityZoneDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of Availability Zones corresponding to the segments in a trace.
--
--
--
-- /See:/ 'availabilityZoneDetail' smart constructor.
newtype AvailabilityZoneDetail = AvailabilityZoneDetail'
  { _azdName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZoneDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azdName' - The name of a corresponding Availability Zone.
availabilityZoneDetail ::
  AvailabilityZoneDetail
availabilityZoneDetail =
  AvailabilityZoneDetail' {_azdName = Nothing}

-- | The name of a corresponding Availability Zone.
azdName :: Lens' AvailabilityZoneDetail (Maybe Text)
azdName = lens _azdName (\s a -> s {_azdName = a})

instance FromJSON AvailabilityZoneDetail where
  parseJSON =
    withObject
      "AvailabilityZoneDetail"
      (\x -> AvailabilityZoneDetail' <$> (x .:? "Name"))

instance Hashable AvailabilityZoneDetail

instance NFData AvailabilityZoneDetail
