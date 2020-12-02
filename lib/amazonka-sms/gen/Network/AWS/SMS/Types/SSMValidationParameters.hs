{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.SSMValidationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.SSMValidationParameters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Source

-- | Contains validation parameters.
--
--
--
-- /See:/ 'sSMValidationParameters' smart constructor.
data SSMValidationParameters = SSMValidationParameters'
  { _ssmvpInstanceId ::
      !(Maybe Text),
    _ssmvpCommand :: !(Maybe Text),
    _ssmvpExecutionTimeoutSeconds ::
      !(Maybe Nat),
    _ssmvpScriptType :: !(Maybe ScriptType),
    _ssmvpSource :: !(Maybe Source),
    _ssmvpOutputS3BucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSMValidationParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssmvpInstanceId' - The ID of the instance. The instance must have the following tag: UserForSMSApplicationValidation=true.
--
-- * 'ssmvpCommand' - The command to run the validation script
--
-- * 'ssmvpExecutionTimeoutSeconds' - The timeout interval, in seconds.
--
-- * 'ssmvpScriptType' - The type of validation script.
--
-- * 'ssmvpSource' - The location of the validation script.
--
-- * 'ssmvpOutputS3BucketName' - The name of the S3 bucket for output.
sSMValidationParameters ::
  SSMValidationParameters
sSMValidationParameters =
  SSMValidationParameters'
    { _ssmvpInstanceId = Nothing,
      _ssmvpCommand = Nothing,
      _ssmvpExecutionTimeoutSeconds = Nothing,
      _ssmvpScriptType = Nothing,
      _ssmvpSource = Nothing,
      _ssmvpOutputS3BucketName = Nothing
    }

-- | The ID of the instance. The instance must have the following tag: UserForSMSApplicationValidation=true.
ssmvpInstanceId :: Lens' SSMValidationParameters (Maybe Text)
ssmvpInstanceId = lens _ssmvpInstanceId (\s a -> s {_ssmvpInstanceId = a})

-- | The command to run the validation script
ssmvpCommand :: Lens' SSMValidationParameters (Maybe Text)
ssmvpCommand = lens _ssmvpCommand (\s a -> s {_ssmvpCommand = a})

-- | The timeout interval, in seconds.
ssmvpExecutionTimeoutSeconds :: Lens' SSMValidationParameters (Maybe Natural)
ssmvpExecutionTimeoutSeconds = lens _ssmvpExecutionTimeoutSeconds (\s a -> s {_ssmvpExecutionTimeoutSeconds = a}) . mapping _Nat

-- | The type of validation script.
ssmvpScriptType :: Lens' SSMValidationParameters (Maybe ScriptType)
ssmvpScriptType = lens _ssmvpScriptType (\s a -> s {_ssmvpScriptType = a})

-- | The location of the validation script.
ssmvpSource :: Lens' SSMValidationParameters (Maybe Source)
ssmvpSource = lens _ssmvpSource (\s a -> s {_ssmvpSource = a})

-- | The name of the S3 bucket for output.
ssmvpOutputS3BucketName :: Lens' SSMValidationParameters (Maybe Text)
ssmvpOutputS3BucketName = lens _ssmvpOutputS3BucketName (\s a -> s {_ssmvpOutputS3BucketName = a})

instance FromJSON SSMValidationParameters where
  parseJSON =
    withObject
      "SSMValidationParameters"
      ( \x ->
          SSMValidationParameters'
            <$> (x .:? "instanceId")
            <*> (x .:? "command")
            <*> (x .:? "executionTimeoutSeconds")
            <*> (x .:? "scriptType")
            <*> (x .:? "source")
            <*> (x .:? "outputS3BucketName")
      )

instance Hashable SSMValidationParameters

instance NFData SSMValidationParameters

instance ToJSON SSMValidationParameters where
  toJSON SSMValidationParameters' {..} =
    object
      ( catMaybes
          [ ("instanceId" .=) <$> _ssmvpInstanceId,
            ("command" .=) <$> _ssmvpCommand,
            ("executionTimeoutSeconds" .=) <$> _ssmvpExecutionTimeoutSeconds,
            ("scriptType" .=) <$> _ssmvpScriptType,
            ("source" .=) <$> _ssmvpSource,
            ("outputS3BucketName" .=) <$> _ssmvpOutputS3BucketName
          ]
      )
