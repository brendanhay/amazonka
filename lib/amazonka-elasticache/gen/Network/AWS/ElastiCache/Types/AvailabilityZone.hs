{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AvailabilityZone where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Availability Zone in which the cluster is launched.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone' {_azName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the Availability Zone.
availabilityZone ::
  AvailabilityZone
availabilityZone = AvailabilityZone' {_azName = Nothing}

-- | The name of the Availability Zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s {_azName = a})

instance FromXML AvailabilityZone where
  parseXML x = AvailabilityZone' <$> (x .@? "Name")

instance Hashable AvailabilityZone

instance NFData AvailabilityZone
