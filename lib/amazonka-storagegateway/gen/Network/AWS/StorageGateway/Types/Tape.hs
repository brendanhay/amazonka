{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Tape
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.Tape where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a virtual tape object.
--
--
--
-- /See:/ 'tape' smart constructor.
data Tape = Tape'
  { _tTapeBarcode :: !(Maybe Text),
    _tTapeStatus :: !(Maybe Text),
    _tKMSKey :: !(Maybe Text),
    _tTapeARN :: !(Maybe Text),
    _tProgress :: !(Maybe Double),
    _tTapeSizeInBytes :: !(Maybe Integer),
    _tVTLDevice :: !(Maybe Text),
    _tPoolId :: !(Maybe Text),
    _tTapeUsedInBytes :: !(Maybe Integer),
    _tTapeCreatedDate :: !(Maybe POSIX),
    _tPoolEntryDate :: !(Maybe POSIX),
    _tWorm :: !(Maybe Bool),
    _tRetentionStartDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tape' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTapeBarcode' - The barcode that identifies a specific virtual tape.
--
-- * 'tTapeStatus' - The current state of the virtual tape.
--
-- * 'tKMSKey' - Undocumented member.
--
-- * 'tTapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
--
-- * 'tProgress' - For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete. Range: 0 (not started) to 100 (complete).
--
-- * 'tTapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
--
-- * 'tVTLDevice' - The virtual tape library (VTL) device that the virtual tape is associated with.
--
-- * 'tPoolId' - The ID of the pool that contains tapes that will be archived. The tapes in this pool are archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- * 'tTapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
--
-- * 'tTapeCreatedDate' - The date the virtual tape was created.
--
-- * 'tPoolEntryDate' - The date that the tape enters a custom tape pool.
--
-- * 'tWorm' - If the tape is archived as write-once-read-many (WORM), this value is @true@ .
--
-- * 'tRetentionStartDate' - The date that the tape is first archived with tape retention lock enabled.
tape ::
  Tape
tape =
  Tape'
    { _tTapeBarcode = Nothing,
      _tTapeStatus = Nothing,
      _tKMSKey = Nothing,
      _tTapeARN = Nothing,
      _tProgress = Nothing,
      _tTapeSizeInBytes = Nothing,
      _tVTLDevice = Nothing,
      _tPoolId = Nothing,
      _tTapeUsedInBytes = Nothing,
      _tTapeCreatedDate = Nothing,
      _tPoolEntryDate = Nothing,
      _tWorm = Nothing,
      _tRetentionStartDate = Nothing
    }

-- | The barcode that identifies a specific virtual tape.
tTapeBarcode :: Lens' Tape (Maybe Text)
tTapeBarcode = lens _tTapeBarcode (\s a -> s {_tTapeBarcode = a})

-- | The current state of the virtual tape.
tTapeStatus :: Lens' Tape (Maybe Text)
tTapeStatus = lens _tTapeStatus (\s a -> s {_tTapeStatus = a})

-- | Undocumented member.
tKMSKey :: Lens' Tape (Maybe Text)
tKMSKey = lens _tKMSKey (\s a -> s {_tKMSKey = a})

-- | The Amazon Resource Name (ARN) of the virtual tape.
tTapeARN :: Lens' Tape (Maybe Text)
tTapeARN = lens _tTapeARN (\s a -> s {_tTapeARN = a})

-- | For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete. Range: 0 (not started) to 100 (complete).
tProgress :: Lens' Tape (Maybe Double)
tProgress = lens _tProgress (\s a -> s {_tProgress = a})

-- | The size, in bytes, of the virtual tape capacity.
tTapeSizeInBytes :: Lens' Tape (Maybe Integer)
tTapeSizeInBytes = lens _tTapeSizeInBytes (\s a -> s {_tTapeSizeInBytes = a})

-- | The virtual tape library (VTL) device that the virtual tape is associated with.
tVTLDevice :: Lens' Tape (Maybe Text)
tVTLDevice = lens _tVTLDevice (\s a -> s {_tVTLDevice = a})

-- | The ID of the pool that contains tapes that will be archived. The tapes in this pool are archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
tPoolId :: Lens' Tape (Maybe Text)
tPoolId = lens _tPoolId (\s a -> s {_tPoolId = a})

-- | The size, in bytes, of data stored on the virtual tape.
tTapeUsedInBytes :: Lens' Tape (Maybe Integer)
tTapeUsedInBytes = lens _tTapeUsedInBytes (\s a -> s {_tTapeUsedInBytes = a})

-- | The date the virtual tape was created.
tTapeCreatedDate :: Lens' Tape (Maybe UTCTime)
tTapeCreatedDate = lens _tTapeCreatedDate (\s a -> s {_tTapeCreatedDate = a}) . mapping _Time

-- | The date that the tape enters a custom tape pool.
tPoolEntryDate :: Lens' Tape (Maybe UTCTime)
tPoolEntryDate = lens _tPoolEntryDate (\s a -> s {_tPoolEntryDate = a}) . mapping _Time

-- | If the tape is archived as write-once-read-many (WORM), this value is @true@ .
tWorm :: Lens' Tape (Maybe Bool)
tWorm = lens _tWorm (\s a -> s {_tWorm = a})

-- | The date that the tape is first archived with tape retention lock enabled.
tRetentionStartDate :: Lens' Tape (Maybe UTCTime)
tRetentionStartDate = lens _tRetentionStartDate (\s a -> s {_tRetentionStartDate = a}) . mapping _Time

instance FromJSON Tape where
  parseJSON =
    withObject
      "Tape"
      ( \x ->
          Tape'
            <$> (x .:? "TapeBarcode")
            <*> (x .:? "TapeStatus")
            <*> (x .:? "KMSKey")
            <*> (x .:? "TapeARN")
            <*> (x .:? "Progress")
            <*> (x .:? "TapeSizeInBytes")
            <*> (x .:? "VTLDevice")
            <*> (x .:? "PoolId")
            <*> (x .:? "TapeUsedInBytes")
            <*> (x .:? "TapeCreatedDate")
            <*> (x .:? "PoolEntryDate")
            <*> (x .:? "Worm")
            <*> (x .:? "RetentionStartDate")
      )

instance Hashable Tape

instance NFData Tape
