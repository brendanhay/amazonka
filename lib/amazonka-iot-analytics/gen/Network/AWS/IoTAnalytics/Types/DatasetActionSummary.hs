{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetActionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetActionSummary where

import Network.AWS.IoTAnalytics.Types.DatasetActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the action that automatically creates the dataset's contents.
--
--
--
-- /See:/ 'datasetActionSummary' smart constructor.
data DatasetActionSummary = DatasetActionSummary'
  { _dasActionName ::
      !(Maybe Text),
    _dasActionType :: !(Maybe DatasetActionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetActionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasActionName' - The name of the action that automatically creates the dataset's contents.
--
-- * 'dasActionType' - The type of action by which the dataset's contents are automatically created.
datasetActionSummary ::
  DatasetActionSummary
datasetActionSummary =
  DatasetActionSummary'
    { _dasActionName = Nothing,
      _dasActionType = Nothing
    }

-- | The name of the action that automatically creates the dataset's contents.
dasActionName :: Lens' DatasetActionSummary (Maybe Text)
dasActionName = lens _dasActionName (\s a -> s {_dasActionName = a})

-- | The type of action by which the dataset's contents are automatically created.
dasActionType :: Lens' DatasetActionSummary (Maybe DatasetActionType)
dasActionType = lens _dasActionType (\s a -> s {_dasActionType = a})

instance FromJSON DatasetActionSummary where
  parseJSON =
    withObject
      "DatasetActionSummary"
      ( \x ->
          DatasetActionSummary'
            <$> (x .:? "actionName") <*> (x .:? "actionType")
      )

instance Hashable DatasetActionSummary

instance NFData DatasetActionSummary
