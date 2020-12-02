{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.S3DestinationAccessControl
import Network.AWS.MediaConvert.Types.S3EncryptionSettings
import Network.AWS.Prelude

-- | Settings associated with S3 destination
--
-- /See:/ 's3DestinationSettings' smart constructor.
data S3DestinationSettings = S3DestinationSettings'
  { _sdsAccessControl ::
      !(Maybe S3DestinationAccessControl),
    _sdsEncryption :: !(Maybe S3EncryptionSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3DestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsAccessControl' - Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
--
-- * 'sdsEncryption' - Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
s3DestinationSettings ::
  S3DestinationSettings
s3DestinationSettings =
  S3DestinationSettings'
    { _sdsAccessControl = Nothing,
      _sdsEncryption = Nothing
    }

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
sdsAccessControl :: Lens' S3DestinationSettings (Maybe S3DestinationAccessControl)
sdsAccessControl = lens _sdsAccessControl (\s a -> s {_sdsAccessControl = a})

-- | Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
sdsEncryption :: Lens' S3DestinationSettings (Maybe S3EncryptionSettings)
sdsEncryption = lens _sdsEncryption (\s a -> s {_sdsEncryption = a})

instance FromJSON S3DestinationSettings where
  parseJSON =
    withObject
      "S3DestinationSettings"
      ( \x ->
          S3DestinationSettings'
            <$> (x .:? "accessControl") <*> (x .:? "encryption")
      )

instance Hashable S3DestinationSettings

instance NFData S3DestinationSettings

instance ToJSON S3DestinationSettings where
  toJSON S3DestinationSettings' {..} =
    object
      ( catMaybes
          [ ("accessControl" .=) <$> _sdsAccessControl,
            ("encryption" .=) <$> _sdsEncryption
          ]
      )
