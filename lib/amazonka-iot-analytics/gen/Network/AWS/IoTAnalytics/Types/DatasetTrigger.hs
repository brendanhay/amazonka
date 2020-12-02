{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetTrigger where

import Network.AWS.IoTAnalytics.Types.Schedule
import Network.AWS.IoTAnalytics.Types.TriggeringDataset
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @DatasetTrigger@ that specifies when the data set is automatically updated.
--
--
--
-- /See:/ 'datasetTrigger' smart constructor.
data DatasetTrigger = DatasetTrigger'
  { _dtDataset ::
      !(Maybe TriggeringDataset),
    _dtSchedule :: !(Maybe Schedule)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtDataset' - The data set whose content creation triggers the creation of this data set's contents.
--
-- * 'dtSchedule' - The Schedule when the trigger is initiated.
datasetTrigger ::
  DatasetTrigger
datasetTrigger =
  DatasetTrigger' {_dtDataset = Nothing, _dtSchedule = Nothing}

-- | The data set whose content creation triggers the creation of this data set's contents.
dtDataset :: Lens' DatasetTrigger (Maybe TriggeringDataset)
dtDataset = lens _dtDataset (\s a -> s {_dtDataset = a})

-- | The Schedule when the trigger is initiated.
dtSchedule :: Lens' DatasetTrigger (Maybe Schedule)
dtSchedule = lens _dtSchedule (\s a -> s {_dtSchedule = a})

instance FromJSON DatasetTrigger where
  parseJSON =
    withObject
      "DatasetTrigger"
      ( \x ->
          DatasetTrigger' <$> (x .:? "dataset") <*> (x .:? "schedule")
      )

instance Hashable DatasetTrigger

instance NFData DatasetTrigger

instance ToJSON DatasetTrigger where
  toJSON DatasetTrigger' {..} =
    object
      ( catMaybes
          [("dataset" .=) <$> _dtDataset, ("schedule" .=) <$> _dtSchedule]
      )
