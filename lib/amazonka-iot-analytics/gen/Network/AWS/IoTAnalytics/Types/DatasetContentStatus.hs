{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentStatus where

import Network.AWS.IoTAnalytics.Types.DatasetContentState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The state of the data set contents and the reason they are in this state.
--
--
--
-- /See:/ 'datasetContentStatus' smart constructor.
data DatasetContentStatus = DatasetContentStatus'
  { _dcsState ::
      !(Maybe DatasetContentState),
    _dcsReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetContentStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsState' - The state of the data set contents. Can be one of READY, CREATING, SUCCEEDED, or FAILED.
--
-- * 'dcsReason' - The reason the data set contents are in this state.
datasetContentStatus ::
  DatasetContentStatus
datasetContentStatus =
  DatasetContentStatus' {_dcsState = Nothing, _dcsReason = Nothing}

-- | The state of the data set contents. Can be one of READY, CREATING, SUCCEEDED, or FAILED.
dcsState :: Lens' DatasetContentStatus (Maybe DatasetContentState)
dcsState = lens _dcsState (\s a -> s {_dcsState = a})

-- | The reason the data set contents are in this state.
dcsReason :: Lens' DatasetContentStatus (Maybe Text)
dcsReason = lens _dcsReason (\s a -> s {_dcsReason = a})

instance FromJSON DatasetContentStatus where
  parseJSON =
    withObject
      "DatasetContentStatus"
      ( \x ->
          DatasetContentStatus' <$> (x .:? "state") <*> (x .:? "reason")
      )

instance Hashable DatasetContentStatus

instance NFData DatasetContentStatus
