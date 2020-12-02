{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of a daily automated snapshot.
--
--
--
-- /See:/ 'snapshotOptionsStatus' smart constructor.
data SnapshotOptionsStatus = SnapshotOptionsStatus'
  { _sosOptions ::
      !SnapshotOptions,
    _sosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sosOptions' - Specifies the daily snapshot options specified for the Elasticsearch domain.
--
-- * 'sosStatus' - Specifies the status of a daily automated snapshot.
snapshotOptionsStatus ::
  -- | 'sosOptions'
  SnapshotOptions ->
  -- | 'sosStatus'
  OptionStatus ->
  SnapshotOptionsStatus
snapshotOptionsStatus pOptions_ pStatus_ =
  SnapshotOptionsStatus'
    { _sosOptions = pOptions_,
      _sosStatus = pStatus_
    }

-- | Specifies the daily snapshot options specified for the Elasticsearch domain.
sosOptions :: Lens' SnapshotOptionsStatus SnapshotOptions
sosOptions = lens _sosOptions (\s a -> s {_sosOptions = a})

-- | Specifies the status of a daily automated snapshot.
sosStatus :: Lens' SnapshotOptionsStatus OptionStatus
sosStatus = lens _sosStatus (\s a -> s {_sosStatus = a})

instance FromJSON SnapshotOptionsStatus where
  parseJSON =
    withObject
      "SnapshotOptionsStatus"
      ( \x ->
          SnapshotOptionsStatus' <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable SnapshotOptionsStatus

instance NFData SnapshotOptionsStatus
