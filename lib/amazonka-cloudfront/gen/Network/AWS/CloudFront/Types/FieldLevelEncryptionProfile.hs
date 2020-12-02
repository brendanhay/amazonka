{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type for field-level encryption profiles.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfile' smart constructor.
data FieldLevelEncryptionProfile = FieldLevelEncryptionProfile'
  { _flepId ::
      !Text,
    _flepLastModifiedTime :: !ISO8601,
    _flepFieldLevelEncryptionProfileConfig ::
      !FieldLevelEncryptionProfileConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flepId' - The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- * 'flepLastModifiedTime' - The last time the field-level encryption profile was updated.
--
-- * 'flepFieldLevelEncryptionProfileConfig' - A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
fieldLevelEncryptionProfile ::
  -- | 'flepId'
  Text ->
  -- | 'flepLastModifiedTime'
  UTCTime ->
  -- | 'flepFieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  FieldLevelEncryptionProfile
fieldLevelEncryptionProfile
  pId_
  pLastModifiedTime_
  pFieldLevelEncryptionProfileConfig_ =
    FieldLevelEncryptionProfile'
      { _flepId = pId_,
        _flepLastModifiedTime = _Time # pLastModifiedTime_,
        _flepFieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_
      }

-- | The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
flepId :: Lens' FieldLevelEncryptionProfile Text
flepId = lens _flepId (\s a -> s {_flepId = a})

-- | The last time the field-level encryption profile was updated.
flepLastModifiedTime :: Lens' FieldLevelEncryptionProfile UTCTime
flepLastModifiedTime = lens _flepLastModifiedTime (\s a -> s {_flepLastModifiedTime = a}) . _Time

-- | A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
flepFieldLevelEncryptionProfileConfig :: Lens' FieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
flepFieldLevelEncryptionProfileConfig = lens _flepFieldLevelEncryptionProfileConfig (\s a -> s {_flepFieldLevelEncryptionProfileConfig = a})

instance FromXML FieldLevelEncryptionProfile where
  parseXML x =
    FieldLevelEncryptionProfile'
      <$> (x .@ "Id")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "FieldLevelEncryptionProfileConfig")

instance Hashable FieldLevelEncryptionProfile

instance NFData FieldLevelEncryptionProfile
