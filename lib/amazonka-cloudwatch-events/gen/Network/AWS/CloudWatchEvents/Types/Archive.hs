{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Archive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Archive where

import Network.AWS.CloudWatchEvents.Types.ArchiveState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An @Archive@ object that contains details about an archive.
--
--
--
-- /See:/ 'archive' smart constructor.
data Archive = Archive'
  { _aCreationTime :: !(Maybe POSIX),
    _aSizeBytes :: !(Maybe Integer),
    _aEventSourceARN :: !(Maybe Text),
    _aState :: !(Maybe ArchiveState),
    _aEventCount :: !(Maybe Integer),
    _aArchiveName :: !(Maybe Text),
    _aRetentionDays :: !(Maybe Nat),
    _aStateReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Archive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aCreationTime' - The time stamp for the time that the archive was created.
--
-- * 'aSizeBytes' - The size of the archive, in bytes.
--
-- * 'aEventSourceARN' - The ARN of the event bus associated with the archive. Only events from this event bus are sent to the archive.
--
-- * 'aState' - The current state of the archive.
--
-- * 'aEventCount' - The number of events in the archive.
--
-- * 'aArchiveName' - The name of the archive.
--
-- * 'aRetentionDays' - The number of days to retain events in the archive before they are deleted.
--
-- * 'aStateReason' - A description for the reason that the archive is in the current state.
archive ::
  Archive
archive =
  Archive'
    { _aCreationTime = Nothing,
      _aSizeBytes = Nothing,
      _aEventSourceARN = Nothing,
      _aState = Nothing,
      _aEventCount = Nothing,
      _aArchiveName = Nothing,
      _aRetentionDays = Nothing,
      _aStateReason = Nothing
    }

-- | The time stamp for the time that the archive was created.
aCreationTime :: Lens' Archive (Maybe UTCTime)
aCreationTime = lens _aCreationTime (\s a -> s {_aCreationTime = a}) . mapping _Time

-- | The size of the archive, in bytes.
aSizeBytes :: Lens' Archive (Maybe Integer)
aSizeBytes = lens _aSizeBytes (\s a -> s {_aSizeBytes = a})

-- | The ARN of the event bus associated with the archive. Only events from this event bus are sent to the archive.
aEventSourceARN :: Lens' Archive (Maybe Text)
aEventSourceARN = lens _aEventSourceARN (\s a -> s {_aEventSourceARN = a})

-- | The current state of the archive.
aState :: Lens' Archive (Maybe ArchiveState)
aState = lens _aState (\s a -> s {_aState = a})

-- | The number of events in the archive.
aEventCount :: Lens' Archive (Maybe Integer)
aEventCount = lens _aEventCount (\s a -> s {_aEventCount = a})

-- | The name of the archive.
aArchiveName :: Lens' Archive (Maybe Text)
aArchiveName = lens _aArchiveName (\s a -> s {_aArchiveName = a})

-- | The number of days to retain events in the archive before they are deleted.
aRetentionDays :: Lens' Archive (Maybe Natural)
aRetentionDays = lens _aRetentionDays (\s a -> s {_aRetentionDays = a}) . mapping _Nat

-- | A description for the reason that the archive is in the current state.
aStateReason :: Lens' Archive (Maybe Text)
aStateReason = lens _aStateReason (\s a -> s {_aStateReason = a})

instance FromJSON Archive where
  parseJSON =
    withObject
      "Archive"
      ( \x ->
          Archive'
            <$> (x .:? "CreationTime")
            <*> (x .:? "SizeBytes")
            <*> (x .:? "EventSourceArn")
            <*> (x .:? "State")
            <*> (x .:? "EventCount")
            <*> (x .:? "ArchiveName")
            <*> (x .:? "RetentionDays")
            <*> (x .:? "StateReason")
      )

instance Hashable Archive

instance NFData Archive
