{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An automatic tape creation policy consists of automatic tape creation rules where each rule defines when and how to create new tapes. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
--
--
--
-- /See:/ 'automaticTapeCreationRule' smart constructor.
data AutomaticTapeCreationRule = AutomaticTapeCreationRule'
  { _atcrWorm ::
      !(Maybe Bool),
    _atcrTapeBarcodePrefix :: !Text,
    _atcrPoolId :: !Text,
    _atcrTapeSizeInBytes :: !Integer,
    _atcrMinimumNumTapes :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomaticTapeCreationRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atcrWorm' - Set to @true@ to indicate that tapes are to be archived as write-once-read-many (WORM). Set to @false@ when WORM is not enabled for tapes.
--
-- * 'atcrTapeBarcodePrefix' - A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
--
-- * 'atcrPoolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- * 'atcrTapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
--
-- * 'atcrMinimumNumTapes' - The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
automaticTapeCreationRule ::
  -- | 'atcrTapeBarcodePrefix'
  Text ->
  -- | 'atcrPoolId'
  Text ->
  -- | 'atcrTapeSizeInBytes'
  Integer ->
  -- | 'atcrMinimumNumTapes'
  Natural ->
  AutomaticTapeCreationRule
automaticTapeCreationRule
  pTapeBarcodePrefix_
  pPoolId_
  pTapeSizeInBytes_
  pMinimumNumTapes_ =
    AutomaticTapeCreationRule'
      { _atcrWorm = Nothing,
        _atcrTapeBarcodePrefix = pTapeBarcodePrefix_,
        _atcrPoolId = pPoolId_,
        _atcrTapeSizeInBytes = pTapeSizeInBytes_,
        _atcrMinimumNumTapes = _Nat # pMinimumNumTapes_
      }

-- | Set to @true@ to indicate that tapes are to be archived as write-once-read-many (WORM). Set to @false@ when WORM is not enabled for tapes.
atcrWorm :: Lens' AutomaticTapeCreationRule (Maybe Bool)
atcrWorm = lens _atcrWorm (\s a -> s {_atcrWorm = a})

-- | A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
atcrTapeBarcodePrefix :: Lens' AutomaticTapeCreationRule Text
atcrTapeBarcodePrefix = lens _atcrTapeBarcodePrefix (\s a -> s {_atcrTapeBarcodePrefix = a})

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
atcrPoolId :: Lens' AutomaticTapeCreationRule Text
atcrPoolId = lens _atcrPoolId (\s a -> s {_atcrPoolId = a})

-- | The size, in bytes, of the virtual tape capacity.
atcrTapeSizeInBytes :: Lens' AutomaticTapeCreationRule Integer
atcrTapeSizeInBytes = lens _atcrTapeSizeInBytes (\s a -> s {_atcrTapeSizeInBytes = a})

-- | The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
atcrMinimumNumTapes :: Lens' AutomaticTapeCreationRule Natural
atcrMinimumNumTapes = lens _atcrMinimumNumTapes (\s a -> s {_atcrMinimumNumTapes = a}) . _Nat

instance FromJSON AutomaticTapeCreationRule where
  parseJSON =
    withObject
      "AutomaticTapeCreationRule"
      ( \x ->
          AutomaticTapeCreationRule'
            <$> (x .:? "Worm")
            <*> (x .: "TapeBarcodePrefix")
            <*> (x .: "PoolId")
            <*> (x .: "TapeSizeInBytes")
            <*> (x .: "MinimumNumTapes")
      )

instance Hashable AutomaticTapeCreationRule

instance NFData AutomaticTapeCreationRule

instance ToJSON AutomaticTapeCreationRule where
  toJSON AutomaticTapeCreationRule' {..} =
    object
      ( catMaybes
          [ ("Worm" .=) <$> _atcrWorm,
            Just ("TapeBarcodePrefix" .= _atcrTapeBarcodePrefix),
            Just ("PoolId" .= _atcrPoolId),
            Just ("TapeSizeInBytes" .= _atcrTapeSizeInBytes),
            Just ("MinimumNumTapes" .= _atcrMinimumNumTapes)
          ]
      )
