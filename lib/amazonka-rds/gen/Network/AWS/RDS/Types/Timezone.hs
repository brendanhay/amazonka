{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Timezone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Timezone where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A time zone associated with a @DBInstance@ or a @DBSnapshot@ . This data type is an element in the response to the @DescribeDBInstances@ , the @DescribeDBSnapshots@ , and the @DescribeDBEngineVersions@ actions.
--
--
--
-- /See:/ 'timezone' smart constructor.
newtype Timezone = Timezone' {_tTimezoneName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Timezone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTimezoneName' - The name of the time zone.
timezone ::
  Timezone
timezone = Timezone' {_tTimezoneName = Nothing}

-- | The name of the time zone.
tTimezoneName :: Lens' Timezone (Maybe Text)
tTimezoneName = lens _tTimezoneName (\s a -> s {_tTimezoneName = a})

instance FromXML Timezone where
  parseXML x = Timezone' <$> (x .@? "TimezoneName")

instance Hashable Timezone

instance NFData Timezone
