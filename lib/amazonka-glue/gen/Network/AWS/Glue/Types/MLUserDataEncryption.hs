{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MLUserDataEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MLUserDataEncryption where

import Network.AWS.Glue.Types.MLUserDataEncryptionModeString
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption-at-rest settings of the transform that apply to accessing user data.
--
--
--
-- /See:/ 'mLUserDataEncryption' smart constructor.
data MLUserDataEncryption = MLUserDataEncryption'
  { _mludeKMSKeyId ::
      !(Maybe Text),
    _mludeMlUserDataEncryptionMode ::
      !MLUserDataEncryptionModeString
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MLUserDataEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mludeKMSKeyId' - The ID for the customer-provided KMS key.
--
-- * 'mludeMlUserDataEncryptionMode' - The encryption mode applied to user data. Valid values are:     * DISABLED: encryption is disabled     * SSEKMS: use of server-side encryption with AWS Key Management Service (SSE-KMS) for user data stored in Amazon S3.
mLUserDataEncryption ::
  -- | 'mludeMlUserDataEncryptionMode'
  MLUserDataEncryptionModeString ->
  MLUserDataEncryption
mLUserDataEncryption pMlUserDataEncryptionMode_ =
  MLUserDataEncryption'
    { _mludeKMSKeyId = Nothing,
      _mludeMlUserDataEncryptionMode = pMlUserDataEncryptionMode_
    }

-- | The ID for the customer-provided KMS key.
mludeKMSKeyId :: Lens' MLUserDataEncryption (Maybe Text)
mludeKMSKeyId = lens _mludeKMSKeyId (\s a -> s {_mludeKMSKeyId = a})

-- | The encryption mode applied to user data. Valid values are:     * DISABLED: encryption is disabled     * SSEKMS: use of server-side encryption with AWS Key Management Service (SSE-KMS) for user data stored in Amazon S3.
mludeMlUserDataEncryptionMode :: Lens' MLUserDataEncryption MLUserDataEncryptionModeString
mludeMlUserDataEncryptionMode = lens _mludeMlUserDataEncryptionMode (\s a -> s {_mludeMlUserDataEncryptionMode = a})

instance FromJSON MLUserDataEncryption where
  parseJSON =
    withObject
      "MLUserDataEncryption"
      ( \x ->
          MLUserDataEncryption'
            <$> (x .:? "KmsKeyId") <*> (x .: "MlUserDataEncryptionMode")
      )

instance Hashable MLUserDataEncryption

instance NFData MLUserDataEncryption

instance ToJSON MLUserDataEncryption where
  toJSON MLUserDataEncryption' {..} =
    object
      ( catMaybes
          [ ("KmsKeyId" .=) <$> _mludeKMSKeyId,
            Just
              ("MlUserDataEncryptionMode" .= _mludeMlUserDataEncryptionMode)
          ]
      )
