{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The datastore activity that specifies where to store the processed data.
--
--
--
-- /See:/ 'datastoreActivity' smart constructor.
data DatastoreActivity = DatastoreActivity'
  { _daName :: !Text,
    _daDatastoreName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatastoreActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daName' - The name of the datastore activity.
--
-- * 'daDatastoreName' - The name of the data store where processed messages are stored.
datastoreActivity ::
  -- | 'daName'
  Text ->
  -- | 'daDatastoreName'
  Text ->
  DatastoreActivity
datastoreActivity pName_ pDatastoreName_ =
  DatastoreActivity'
    { _daName = pName_,
      _daDatastoreName = pDatastoreName_
    }

-- | The name of the datastore activity.
daName :: Lens' DatastoreActivity Text
daName = lens _daName (\s a -> s {_daName = a})

-- | The name of the data store where processed messages are stored.
daDatastoreName :: Lens' DatastoreActivity Text
daDatastoreName = lens _daDatastoreName (\s a -> s {_daDatastoreName = a})

instance FromJSON DatastoreActivity where
  parseJSON =
    withObject
      "DatastoreActivity"
      ( \x ->
          DatastoreActivity' <$> (x .: "name") <*> (x .: "datastoreName")
      )

instance Hashable DatastoreActivity

instance NFData DatastoreActivity

instance ToJSON DatastoreActivity where
  toJSON DatastoreActivity' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _daName),
            Just ("datastoreName" .= _daDatastoreName)
          ]
      )
