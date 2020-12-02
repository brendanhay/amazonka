{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobBookmarksEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarksEncryption where

import Network.AWS.Glue.Types.JobBookmarksEncryptionMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies how job bookmark data should be encrypted.
--
--
--
-- /See:/ 'jobBookmarksEncryption' smart constructor.
data JobBookmarksEncryption = JobBookmarksEncryption'
  { _jbeJobBookmarksEncryptionMode ::
      !(Maybe JobBookmarksEncryptionMode),
    _jbeKMSKeyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobBookmarksEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jbeJobBookmarksEncryptionMode' - The encryption mode to use for job bookmarks data.
--
-- * 'jbeKMSKeyARN' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
jobBookmarksEncryption ::
  JobBookmarksEncryption
jobBookmarksEncryption =
  JobBookmarksEncryption'
    { _jbeJobBookmarksEncryptionMode = Nothing,
      _jbeKMSKeyARN = Nothing
    }

-- | The encryption mode to use for job bookmarks data.
jbeJobBookmarksEncryptionMode :: Lens' JobBookmarksEncryption (Maybe JobBookmarksEncryptionMode)
jbeJobBookmarksEncryptionMode = lens _jbeJobBookmarksEncryptionMode (\s a -> s {_jbeJobBookmarksEncryptionMode = a})

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
jbeKMSKeyARN :: Lens' JobBookmarksEncryption (Maybe Text)
jbeKMSKeyARN = lens _jbeKMSKeyARN (\s a -> s {_jbeKMSKeyARN = a})

instance FromJSON JobBookmarksEncryption where
  parseJSON =
    withObject
      "JobBookmarksEncryption"
      ( \x ->
          JobBookmarksEncryption'
            <$> (x .:? "JobBookmarksEncryptionMode") <*> (x .:? "KmsKeyArn")
      )

instance Hashable JobBookmarksEncryption

instance NFData JobBookmarksEncryption

instance ToJSON JobBookmarksEncryption where
  toJSON JobBookmarksEncryption' {..} =
    object
      ( catMaybes
          [ ("JobBookmarksEncryptionMode" .=)
              <$> _jbeJobBookmarksEncryptionMode,
            ("KmsKeyArn" .=) <$> _jbeKMSKeyARN
          ]
      )
