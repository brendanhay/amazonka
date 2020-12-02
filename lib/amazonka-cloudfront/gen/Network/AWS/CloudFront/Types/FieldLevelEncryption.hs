{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryption where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type that includes the profile configurations and other options specified for field-level encryption.
--
--
--
-- /See:/ 'fieldLevelEncryption' smart constructor.
data FieldLevelEncryption = FieldLevelEncryption'
  { _fleId :: !Text,
    _fleLastModifiedTime :: !ISO8601,
    _fleFieldLevelEncryptionConfig ::
      !FieldLevelEncryptionConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldLevelEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleId' - The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- * 'fleLastModifiedTime' - The last time the field-level encryption configuration was changed.
--
-- * 'fleFieldLevelEncryptionConfig' - A complex data type that includes the profile configurations specified for field-level encryption.
fieldLevelEncryption ::
  -- | 'fleId'
  Text ->
  -- | 'fleLastModifiedTime'
  UTCTime ->
  -- | 'fleFieldLevelEncryptionConfig'
  FieldLevelEncryptionConfig ->
  FieldLevelEncryption
fieldLevelEncryption
  pId_
  pLastModifiedTime_
  pFieldLevelEncryptionConfig_ =
    FieldLevelEncryption'
      { _fleId = pId_,
        _fleLastModifiedTime = _Time # pLastModifiedTime_,
        _fleFieldLevelEncryptionConfig = pFieldLevelEncryptionConfig_
      }

-- | The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
fleId :: Lens' FieldLevelEncryption Text
fleId = lens _fleId (\s a -> s {_fleId = a})

-- | The last time the field-level encryption configuration was changed.
fleLastModifiedTime :: Lens' FieldLevelEncryption UTCTime
fleLastModifiedTime = lens _fleLastModifiedTime (\s a -> s {_fleLastModifiedTime = a}) . _Time

-- | A complex data type that includes the profile configurations specified for field-level encryption.
fleFieldLevelEncryptionConfig :: Lens' FieldLevelEncryption FieldLevelEncryptionConfig
fleFieldLevelEncryptionConfig = lens _fleFieldLevelEncryptionConfig (\s a -> s {_fleFieldLevelEncryptionConfig = a})

instance FromXML FieldLevelEncryption where
  parseXML x =
    FieldLevelEncryption'
      <$> (x .@ "Id")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "FieldLevelEncryptionConfig")

instance Hashable FieldLevelEncryption

instance NFData FieldLevelEncryption
