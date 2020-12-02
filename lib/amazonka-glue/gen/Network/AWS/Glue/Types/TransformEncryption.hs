{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformEncryption where

import Network.AWS.Glue.Types.MLUserDataEncryption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
--
-- Additionally, imported labels and trained transforms can now be encrypted using a customer provided KMS key.
--
--
-- /See:/ 'transformEncryption' smart constructor.
data TransformEncryption = TransformEncryption'
  { _teMlUserDataEncryption ::
      !(Maybe MLUserDataEncryption),
    _teTaskRunSecurityConfigurationName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'teMlUserDataEncryption' - An @MLUserDataEncryption@ object containing the encryption mode and customer-provided KMS key ID.
--
-- * 'teTaskRunSecurityConfigurationName' - The name of the security configuration.
transformEncryption ::
  TransformEncryption
transformEncryption =
  TransformEncryption'
    { _teMlUserDataEncryption = Nothing,
      _teTaskRunSecurityConfigurationName = Nothing
    }

-- | An @MLUserDataEncryption@ object containing the encryption mode and customer-provided KMS key ID.
teMlUserDataEncryption :: Lens' TransformEncryption (Maybe MLUserDataEncryption)
teMlUserDataEncryption = lens _teMlUserDataEncryption (\s a -> s {_teMlUserDataEncryption = a})

-- | The name of the security configuration.
teTaskRunSecurityConfigurationName :: Lens' TransformEncryption (Maybe Text)
teTaskRunSecurityConfigurationName = lens _teTaskRunSecurityConfigurationName (\s a -> s {_teTaskRunSecurityConfigurationName = a})

instance FromJSON TransformEncryption where
  parseJSON =
    withObject
      "TransformEncryption"
      ( \x ->
          TransformEncryption'
            <$> (x .:? "MlUserDataEncryption")
            <*> (x .:? "TaskRunSecurityConfigurationName")
      )

instance Hashable TransformEncryption

instance NFData TransformEncryption

instance ToJSON TransformEncryption where
  toJSON TransformEncryption' {..} =
    object
      ( catMaybes
          [ ("MlUserDataEncryption" .=) <$> _teMlUserDataEncryption,
            ("TaskRunSecurityConfigurationName" .=)
              <$> _teTaskRunSecurityConfigurationName
          ]
      )
