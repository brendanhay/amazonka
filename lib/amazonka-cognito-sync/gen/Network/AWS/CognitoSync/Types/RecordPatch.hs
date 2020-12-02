{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.RecordPatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.RecordPatch where

import Network.AWS.CognitoSync.Types.Operation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An update operation for a record.
--
-- /See:/ 'recordPatch' smart constructor.
data RecordPatch = RecordPatch'
  { _rpDeviceLastModifiedDate ::
      !(Maybe POSIX),
    _rpValue :: !(Maybe Text),
    _rpOp :: !Operation,
    _rpKey :: !Text,
    _rpSyncCount :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecordPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpDeviceLastModifiedDate' - The last modified date of the client device.
--
-- * 'rpValue' - The value associated with the record patch.
--
-- * 'rpOp' - An operation, either replace or remove.
--
-- * 'rpKey' - The key associated with the record patch.
--
-- * 'rpSyncCount' - Last known server sync count for this record. Set to 0 if unknown.
recordPatch ::
  -- | 'rpOp'
  Operation ->
  -- | 'rpKey'
  Text ->
  -- | 'rpSyncCount'
  Integer ->
  RecordPatch
recordPatch pOp_ pKey_ pSyncCount_ =
  RecordPatch'
    { _rpDeviceLastModifiedDate = Nothing,
      _rpValue = Nothing,
      _rpOp = pOp_,
      _rpKey = pKey_,
      _rpSyncCount = pSyncCount_
    }

-- | The last modified date of the client device.
rpDeviceLastModifiedDate :: Lens' RecordPatch (Maybe UTCTime)
rpDeviceLastModifiedDate = lens _rpDeviceLastModifiedDate (\s a -> s {_rpDeviceLastModifiedDate = a}) . mapping _Time

-- | The value associated with the record patch.
rpValue :: Lens' RecordPatch (Maybe Text)
rpValue = lens _rpValue (\s a -> s {_rpValue = a})

-- | An operation, either replace or remove.
rpOp :: Lens' RecordPatch Operation
rpOp = lens _rpOp (\s a -> s {_rpOp = a})

-- | The key associated with the record patch.
rpKey :: Lens' RecordPatch Text
rpKey = lens _rpKey (\s a -> s {_rpKey = a})

-- | Last known server sync count for this record. Set to 0 if unknown.
rpSyncCount :: Lens' RecordPatch Integer
rpSyncCount = lens _rpSyncCount (\s a -> s {_rpSyncCount = a})

instance Hashable RecordPatch

instance NFData RecordPatch

instance ToJSON RecordPatch where
  toJSON RecordPatch' {..} =
    object
      ( catMaybes
          [ ("DeviceLastModifiedDate" .=) <$> _rpDeviceLastModifiedDate,
            ("Value" .=) <$> _rpValue,
            Just ("Op" .= _rpOp),
            Just ("Key" .= _rpKey),
            Just ("SyncCount" .= _rpSyncCount)
          ]
      )
