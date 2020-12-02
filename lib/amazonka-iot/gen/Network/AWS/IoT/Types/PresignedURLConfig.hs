{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PresignedURLConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PresignedURLConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration for pre-signed S3 URLs.
--
--
--
-- /See:/ 'presignedURLConfig' smart constructor.
data PresignedURLConfig = PresignedURLConfig'
  { _pucExpiresInSec ::
      !(Maybe Nat),
    _pucRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PresignedURLConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pucExpiresInSec' - How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
--
-- * 'pucRoleARN' - The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
presignedURLConfig ::
  PresignedURLConfig
presignedURLConfig =
  PresignedURLConfig'
    { _pucExpiresInSec = Nothing,
      _pucRoleARN = Nothing
    }

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
pucExpiresInSec :: Lens' PresignedURLConfig (Maybe Natural)
pucExpiresInSec = lens _pucExpiresInSec (\s a -> s {_pucExpiresInSec = a}) . mapping _Nat

-- | The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
pucRoleARN :: Lens' PresignedURLConfig (Maybe Text)
pucRoleARN = lens _pucRoleARN (\s a -> s {_pucRoleARN = a})

instance FromJSON PresignedURLConfig where
  parseJSON =
    withObject
      "PresignedURLConfig"
      ( \x ->
          PresignedURLConfig'
            <$> (x .:? "expiresInSec") <*> (x .:? "roleArn")
      )

instance Hashable PresignedURLConfig

instance NFData PresignedURLConfig

instance ToJSON PresignedURLConfig where
  toJSON PresignedURLConfig' {..} =
    object
      ( catMaybes
          [ ("expiresInSec" .=) <$> _pucExpiresInSec,
            ("roleArn" .=) <$> _pucRoleARN
          ]
      )
