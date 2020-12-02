{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyProperties where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.EncryptionKey

-- | The properties of the custom terminology.
--
--
--
-- /See:/ 'terminologyProperties' smart constructor.
data TerminologyProperties = TerminologyProperties'
  { _tpSizeBytes ::
      !(Maybe Int),
    _tpLastUpdatedAt :: !(Maybe POSIX),
    _tpARN :: !(Maybe Text),
    _tpTargetLanguageCodes :: !(Maybe [Text]),
    _tpCreatedAt :: !(Maybe POSIX),
    _tpName :: !(Maybe Text),
    _tpSourceLanguageCode :: !(Maybe Text),
    _tpTermCount :: !(Maybe Int),
    _tpEncryptionKey :: !(Maybe EncryptionKey),
    _tpDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TerminologyProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpSizeBytes' - The size of the file used when importing a custom terminology.
--
-- * 'tpLastUpdatedAt' - The time at which the custom terminology was last update, based on the timestamp.
--
-- * 'tpARN' - The Amazon Resource Name (ARN) of the custom terminology.
--
-- * 'tpTargetLanguageCodes' - The language codes for the target languages available with the custom terminology file. All possible target languages are returned in array.
--
-- * 'tpCreatedAt' - The time at which the custom terminology was created, based on the timestamp.
--
-- * 'tpName' - The name of the custom terminology.
--
-- * 'tpSourceLanguageCode' - The language code for the source text of the translation request for which the custom terminology is being used.
--
-- * 'tpTermCount' - The number of terms included in the custom terminology.
--
-- * 'tpEncryptionKey' - The encryption key for the custom terminology.
--
-- * 'tpDescription' - The description of the custom terminology properties.
terminologyProperties ::
  TerminologyProperties
terminologyProperties =
  TerminologyProperties'
    { _tpSizeBytes = Nothing,
      _tpLastUpdatedAt = Nothing,
      _tpARN = Nothing,
      _tpTargetLanguageCodes = Nothing,
      _tpCreatedAt = Nothing,
      _tpName = Nothing,
      _tpSourceLanguageCode = Nothing,
      _tpTermCount = Nothing,
      _tpEncryptionKey = Nothing,
      _tpDescription = Nothing
    }

-- | The size of the file used when importing a custom terminology.
tpSizeBytes :: Lens' TerminologyProperties (Maybe Int)
tpSizeBytes = lens _tpSizeBytes (\s a -> s {_tpSizeBytes = a})

-- | The time at which the custom terminology was last update, based on the timestamp.
tpLastUpdatedAt :: Lens' TerminologyProperties (Maybe UTCTime)
tpLastUpdatedAt = lens _tpLastUpdatedAt (\s a -> s {_tpLastUpdatedAt = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the custom terminology.
tpARN :: Lens' TerminologyProperties (Maybe Text)
tpARN = lens _tpARN (\s a -> s {_tpARN = a})

-- | The language codes for the target languages available with the custom terminology file. All possible target languages are returned in array.
tpTargetLanguageCodes :: Lens' TerminologyProperties [Text]
tpTargetLanguageCodes = lens _tpTargetLanguageCodes (\s a -> s {_tpTargetLanguageCodes = a}) . _Default . _Coerce

-- | The time at which the custom terminology was created, based on the timestamp.
tpCreatedAt :: Lens' TerminologyProperties (Maybe UTCTime)
tpCreatedAt = lens _tpCreatedAt (\s a -> s {_tpCreatedAt = a}) . mapping _Time

-- | The name of the custom terminology.
tpName :: Lens' TerminologyProperties (Maybe Text)
tpName = lens _tpName (\s a -> s {_tpName = a})

-- | The language code for the source text of the translation request for which the custom terminology is being used.
tpSourceLanguageCode :: Lens' TerminologyProperties (Maybe Text)
tpSourceLanguageCode = lens _tpSourceLanguageCode (\s a -> s {_tpSourceLanguageCode = a})

-- | The number of terms included in the custom terminology.
tpTermCount :: Lens' TerminologyProperties (Maybe Int)
tpTermCount = lens _tpTermCount (\s a -> s {_tpTermCount = a})

-- | The encryption key for the custom terminology.
tpEncryptionKey :: Lens' TerminologyProperties (Maybe EncryptionKey)
tpEncryptionKey = lens _tpEncryptionKey (\s a -> s {_tpEncryptionKey = a})

-- | The description of the custom terminology properties.
tpDescription :: Lens' TerminologyProperties (Maybe Text)
tpDescription = lens _tpDescription (\s a -> s {_tpDescription = a})

instance FromJSON TerminologyProperties where
  parseJSON =
    withObject
      "TerminologyProperties"
      ( \x ->
          TerminologyProperties'
            <$> (x .:? "SizeBytes")
            <*> (x .:? "LastUpdatedAt")
            <*> (x .:? "Arn")
            <*> (x .:? "TargetLanguageCodes" .!= mempty)
            <*> (x .:? "CreatedAt")
            <*> (x .:? "Name")
            <*> (x .:? "SourceLanguageCode")
            <*> (x .:? "TermCount")
            <*> (x .:? "EncryptionKey")
            <*> (x .:? "Description")
      )

instance Hashable TerminologyProperties

instance NFData TerminologyProperties
