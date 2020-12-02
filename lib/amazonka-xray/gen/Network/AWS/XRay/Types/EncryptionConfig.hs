{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.EncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.EncryptionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.EncryptionStatus
import Network.AWS.XRay.Types.EncryptionType

-- | A configuration document that specifies encryption configuration settings.
--
--
--
-- /See:/ 'encryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { _ecStatus ::
      !(Maybe EncryptionStatus),
    _ecKeyId :: !(Maybe Text),
    _ecType :: !(Maybe EncryptionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecStatus' - The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
--
-- * 'ecKeyId' - The ID of the customer master key (CMK) used for encryption, if applicable.
--
-- * 'ecType' - The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
encryptionConfig ::
  EncryptionConfig
encryptionConfig =
  EncryptionConfig'
    { _ecStatus = Nothing,
      _ecKeyId = Nothing,
      _ecType = Nothing
    }

-- | The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
ecStatus :: Lens' EncryptionConfig (Maybe EncryptionStatus)
ecStatus = lens _ecStatus (\s a -> s {_ecStatus = a})

-- | The ID of the customer master key (CMK) used for encryption, if applicable.
ecKeyId :: Lens' EncryptionConfig (Maybe Text)
ecKeyId = lens _ecKeyId (\s a -> s {_ecKeyId = a})

-- | The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
ecType :: Lens' EncryptionConfig (Maybe EncryptionType)
ecType = lens _ecType (\s a -> s {_ecType = a})

instance FromJSON EncryptionConfig where
  parseJSON =
    withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            <$> (x .:? "Status") <*> (x .:? "KeyId") <*> (x .:? "Type")
      )

instance Hashable EncryptionConfig

instance NFData EncryptionConfig
