{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a recovery point.
--
--
--
-- /See:/ 'tapeRecoveryPointInfo' smart constructor.
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'
  { _trpiTapeStatus ::
      !(Maybe Text),
    _trpiTapeRecoveryPointTime :: !(Maybe POSIX),
    _trpiTapeARN :: !(Maybe Text),
    _trpiTapeSizeInBytes :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TapeRecoveryPointInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trpiTapeStatus' - The status of the virtual tapes.
--
-- * 'trpiTapeRecoveryPointTime' - The time when the point-in-time view of the virtual tape was replicated for later recovery. The default timestamp format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- * 'trpiTapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
--
-- * 'trpiTapeSizeInBytes' - The size, in bytes, of the virtual tapes to recover.
tapeRecoveryPointInfo ::
  TapeRecoveryPointInfo
tapeRecoveryPointInfo =
  TapeRecoveryPointInfo'
    { _trpiTapeStatus = Nothing,
      _trpiTapeRecoveryPointTime = Nothing,
      _trpiTapeARN = Nothing,
      _trpiTapeSizeInBytes = Nothing
    }

-- | The status of the virtual tapes.
trpiTapeStatus :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeStatus = lens _trpiTapeStatus (\s a -> s {_trpiTapeStatus = a})

-- | The time when the point-in-time view of the virtual tape was replicated for later recovery. The default timestamp format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
trpiTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe UTCTime)
trpiTapeRecoveryPointTime = lens _trpiTapeRecoveryPointTime (\s a -> s {_trpiTapeRecoveryPointTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the virtual tape.
trpiTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeARN = lens _trpiTapeARN (\s a -> s {_trpiTapeARN = a})

-- | The size, in bytes, of the virtual tapes to recover.
trpiTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpiTapeSizeInBytes = lens _trpiTapeSizeInBytes (\s a -> s {_trpiTapeSizeInBytes = a})

instance FromJSON TapeRecoveryPointInfo where
  parseJSON =
    withObject
      "TapeRecoveryPointInfo"
      ( \x ->
          TapeRecoveryPointInfo'
            <$> (x .:? "TapeStatus")
            <*> (x .:? "TapeRecoveryPointTime")
            <*> (x .:? "TapeARN")
            <*> (x .:? "TapeSizeInBytes")
      )

instance Hashable TapeRecoveryPointInfo

instance NFData TapeRecoveryPointInfo
