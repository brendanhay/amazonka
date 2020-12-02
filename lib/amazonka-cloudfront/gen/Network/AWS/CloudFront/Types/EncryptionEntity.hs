{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EncryptionEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EncryptionEntity where

import Network.AWS.CloudFront.Types.FieldPatterns
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Complex data type for field-level encryption profiles that includes the encryption key and field pattern specifications.
--
--
--
-- /See:/ 'encryptionEntity' smart constructor.
data EncryptionEntity = EncryptionEntity'
  { _eePublicKeyId :: !Text,
    _eeProviderId :: !Text,
    _eeFieldPatterns :: !FieldPatterns
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eePublicKeyId' - The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns.
--
-- * 'eeProviderId' - The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
--
-- * 'eeFieldPatterns' - Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive.
encryptionEntity ::
  -- | 'eePublicKeyId'
  Text ->
  -- | 'eeProviderId'
  Text ->
  -- | 'eeFieldPatterns'
  FieldPatterns ->
  EncryptionEntity
encryptionEntity pPublicKeyId_ pProviderId_ pFieldPatterns_ =
  EncryptionEntity'
    { _eePublicKeyId = pPublicKeyId_,
      _eeProviderId = pProviderId_,
      _eeFieldPatterns = pFieldPatterns_
    }

-- | The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns.
eePublicKeyId :: Lens' EncryptionEntity Text
eePublicKeyId = lens _eePublicKeyId (\s a -> s {_eePublicKeyId = a})

-- | The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
eeProviderId :: Lens' EncryptionEntity Text
eeProviderId = lens _eeProviderId (\s a -> s {_eeProviderId = a})

-- | Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive.
eeFieldPatterns :: Lens' EncryptionEntity FieldPatterns
eeFieldPatterns = lens _eeFieldPatterns (\s a -> s {_eeFieldPatterns = a})

instance FromXML EncryptionEntity where
  parseXML x =
    EncryptionEntity'
      <$> (x .@ "PublicKeyId")
      <*> (x .@ "ProviderId")
      <*> (x .@ "FieldPatterns")

instance Hashable EncryptionEntity

instance NFData EncryptionEntity

instance ToXML EncryptionEntity where
  toXML EncryptionEntity' {..} =
    mconcat
      [ "PublicKeyId" @= _eePublicKeyId,
        "ProviderId" @= _eeProviderId,
        "FieldPatterns" @= _eeFieldPatterns
      ]
