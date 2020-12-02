{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Record where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The basic data structure of a dataset.
--
-- /See:/ 'record' smart constructor.
data Record = Record'
  { _rSyncCount :: !(Maybe Integer),
    _rDeviceLastModifiedDate :: !(Maybe POSIX),
    _rLastModifiedDate :: !(Maybe POSIX),
    _rValue :: !(Maybe Text),
    _rKey :: !(Maybe Text),
    _rLastModifiedBy :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rSyncCount' - The server sync count for this record.
--
-- * 'rDeviceLastModifiedDate' - The last modified date of the client device.
--
-- * 'rLastModifiedDate' - The date on which the record was last modified.
--
-- * 'rValue' - The value for the record.
--
-- * 'rKey' - The key for the record.
--
-- * 'rLastModifiedBy' - The user/device that made the last change to this record.
record ::
  Record
record =
  Record'
    { _rSyncCount = Nothing,
      _rDeviceLastModifiedDate = Nothing,
      _rLastModifiedDate = Nothing,
      _rValue = Nothing,
      _rKey = Nothing,
      _rLastModifiedBy = Nothing
    }

-- | The server sync count for this record.
rSyncCount :: Lens' Record (Maybe Integer)
rSyncCount = lens _rSyncCount (\s a -> s {_rSyncCount = a})

-- | The last modified date of the client device.
rDeviceLastModifiedDate :: Lens' Record (Maybe UTCTime)
rDeviceLastModifiedDate = lens _rDeviceLastModifiedDate (\s a -> s {_rDeviceLastModifiedDate = a}) . mapping _Time

-- | The date on which the record was last modified.
rLastModifiedDate :: Lens' Record (Maybe UTCTime)
rLastModifiedDate = lens _rLastModifiedDate (\s a -> s {_rLastModifiedDate = a}) . mapping _Time

-- | The value for the record.
rValue :: Lens' Record (Maybe Text)
rValue = lens _rValue (\s a -> s {_rValue = a})

-- | The key for the record.
rKey :: Lens' Record (Maybe Text)
rKey = lens _rKey (\s a -> s {_rKey = a})

-- | The user/device that made the last change to this record.
rLastModifiedBy :: Lens' Record (Maybe Text)
rLastModifiedBy = lens _rLastModifiedBy (\s a -> s {_rLastModifiedBy = a})

instance FromJSON Record where
  parseJSON =
    withObject
      "Record"
      ( \x ->
          Record'
            <$> (x .:? "SyncCount")
            <*> (x .:? "DeviceLastModifiedDate")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "Value")
            <*> (x .:? "Key")
            <*> (x .:? "LastModifiedBy")
      )

instance Hashable Record

instance NFData Record
