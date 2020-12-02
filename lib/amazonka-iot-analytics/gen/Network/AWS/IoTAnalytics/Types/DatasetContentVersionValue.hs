{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The dataset whose latest contents are used as input to the notebook or application.
--
--
--
-- /See:/ 'datasetContentVersionValue' smart constructor.
newtype DatasetContentVersionValue = DatasetContentVersionValue'
  { _dcvvDatasetName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetContentVersionValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvvDatasetName' - The name of the dataset whose latest contents are used as input to the notebook or application.
datasetContentVersionValue ::
  -- | 'dcvvDatasetName'
  Text ->
  DatasetContentVersionValue
datasetContentVersionValue pDatasetName_ =
  DatasetContentVersionValue' {_dcvvDatasetName = pDatasetName_}

-- | The name of the dataset whose latest contents are used as input to the notebook or application.
dcvvDatasetName :: Lens' DatasetContentVersionValue Text
dcvvDatasetName = lens _dcvvDatasetName (\s a -> s {_dcvvDatasetName = a})

instance FromJSON DatasetContentVersionValue where
  parseJSON =
    withObject
      "DatasetContentVersionValue"
      (\x -> DatasetContentVersionValue' <$> (x .: "datasetName"))

instance Hashable DatasetContentVersionValue

instance NFData DatasetContentVersionValue

instance ToJSON DatasetContentVersionValue where
  toJSON DatasetContentVersionValue' {..} =
    object (catMaybes [Just ("datasetName" .= _dcvvDatasetName)])
