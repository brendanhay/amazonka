{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStatistics where

import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Statistical information about the data store.
--
--
--
-- /See:/ 'datastoreStatistics' smart constructor.
newtype DatastoreStatistics = DatastoreStatistics'
  { _dsSize ::
      Maybe EstimatedResourceSize
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatastoreStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSize' - The estimated size of the data store.
datastoreStatistics ::
  DatastoreStatistics
datastoreStatistics = DatastoreStatistics' {_dsSize = Nothing}

-- | The estimated size of the data store.
dsSize :: Lens' DatastoreStatistics (Maybe EstimatedResourceSize)
dsSize = lens _dsSize (\s a -> s {_dsSize = a})

instance FromJSON DatastoreStatistics where
  parseJSON =
    withObject
      "DatastoreStatistics"
      (\x -> DatastoreStatistics' <$> (x .:? "size"))

instance Hashable DatastoreStatistics

instance NFData DatastoreStatistics
