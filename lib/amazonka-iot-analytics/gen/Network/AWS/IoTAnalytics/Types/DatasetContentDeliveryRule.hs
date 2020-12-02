{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule where

import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
import Network.AWS.Lens
import Network.AWS.Prelude

-- | When dataset contents are created, they are delivered to destination specified here.
--
--
--
-- /See:/ 'datasetContentDeliveryRule' smart constructor.
data DatasetContentDeliveryRule = DatasetContentDeliveryRule'
  { _dcdrEntryName ::
      !(Maybe Text),
    _dcdrDestination ::
      !DatasetContentDeliveryDestination
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetContentDeliveryRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdrEntryName' - The name of the dataset content delivery rules entry.
--
-- * 'dcdrDestination' - The destination to which dataset contents are delivered.
datasetContentDeliveryRule ::
  -- | 'dcdrDestination'
  DatasetContentDeliveryDestination ->
  DatasetContentDeliveryRule
datasetContentDeliveryRule pDestination_ =
  DatasetContentDeliveryRule'
    { _dcdrEntryName = Nothing,
      _dcdrDestination = pDestination_
    }

-- | The name of the dataset content delivery rules entry.
dcdrEntryName :: Lens' DatasetContentDeliveryRule (Maybe Text)
dcdrEntryName = lens _dcdrEntryName (\s a -> s {_dcdrEntryName = a})

-- | The destination to which dataset contents are delivered.
dcdrDestination :: Lens' DatasetContentDeliveryRule DatasetContentDeliveryDestination
dcdrDestination = lens _dcdrDestination (\s a -> s {_dcdrDestination = a})

instance FromJSON DatasetContentDeliveryRule where
  parseJSON =
    withObject
      "DatasetContentDeliveryRule"
      ( \x ->
          DatasetContentDeliveryRule'
            <$> (x .:? "entryName") <*> (x .: "destination")
      )

instance Hashable DatasetContentDeliveryRule

instance NFData DatasetContentDeliveryRule

instance ToJSON DatasetContentDeliveryRule where
  toJSON DatasetContentDeliveryRule' {..} =
    object
      ( catMaybes
          [ ("entryName" .=) <$> _dcdrEntryName,
            Just ("destination" .= _dcdrDestination)
          ]
      )
