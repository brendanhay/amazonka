{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SnapshotOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
--
--
--
-- /See:/ 'snapshotOptions' smart constructor.
newtype SnapshotOptions = SnapshotOptions'
  { _soAutomatedSnapshotStartHour ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soAutomatedSnapshotStartHour' - Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
snapshotOptions ::
  SnapshotOptions
snapshotOptions =
  SnapshotOptions' {_soAutomatedSnapshotStartHour = Nothing}

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
soAutomatedSnapshotStartHour :: Lens' SnapshotOptions (Maybe Int)
soAutomatedSnapshotStartHour = lens _soAutomatedSnapshotStartHour (\s a -> s {_soAutomatedSnapshotStartHour = a})

instance FromJSON SnapshotOptions where
  parseJSON =
    withObject
      "SnapshotOptions"
      (\x -> SnapshotOptions' <$> (x .:? "AutomatedSnapshotStartHour"))

instance Hashable SnapshotOptions

instance NFData SnapshotOptions

instance ToJSON SnapshotOptions where
  toJSON SnapshotOptions' {..} =
    object
      ( catMaybes
          [ ("AutomatedSnapshotStartHour" .=)
              <$> _soAutomatedSnapshotStartHour
          ]
      )
