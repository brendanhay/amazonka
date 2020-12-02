{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationRecorder where

import Network.AWS.Config.Types.RecordingGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that represents the recording of configuration changes of an AWS resource.
--
--
--
-- /See:/ 'configurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
  { _crName ::
      !(Maybe Text),
    _crRecordingGroup :: !(Maybe RecordingGroup),
    _crRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationRecorder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crName' - The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
--
-- * 'crRecordingGroup' - Specifies the types of AWS resources for which AWS Config records configuration changes.
--
-- * 'crRoleARN' - Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
configurationRecorder ::
  ConfigurationRecorder
configurationRecorder =
  ConfigurationRecorder'
    { _crName = Nothing,
      _crRecordingGroup = Nothing,
      _crRoleARN = Nothing
    }

-- | The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
crName :: Lens' ConfigurationRecorder (Maybe Text)
crName = lens _crName (\s a -> s {_crName = a})

-- | Specifies the types of AWS resources for which AWS Config records configuration changes.
crRecordingGroup :: Lens' ConfigurationRecorder (Maybe RecordingGroup)
crRecordingGroup = lens _crRecordingGroup (\s a -> s {_crRecordingGroup = a})

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
crRoleARN :: Lens' ConfigurationRecorder (Maybe Text)
crRoleARN = lens _crRoleARN (\s a -> s {_crRoleARN = a})

instance FromJSON ConfigurationRecorder where
  parseJSON =
    withObject
      "ConfigurationRecorder"
      ( \x ->
          ConfigurationRecorder'
            <$> (x .:? "name") <*> (x .:? "recordingGroup") <*> (x .:? "roleARN")
      )

instance Hashable ConfigurationRecorder

instance NFData ConfigurationRecorder

instance ToJSON ConfigurationRecorder where
  toJSON ConfigurationRecorder' {..} =
    object
      ( catMaybes
          [ ("name" .=) <$> _crName,
            ("recordingGroup" .=) <$> _crRecordingGroup,
            ("roleARN" .=) <$> _crRoleARN
          ]
      )
