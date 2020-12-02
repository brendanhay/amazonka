{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationConfiguration where

import Network.AWS.Config.Types.ExecutionControls
import Network.AWS.Config.Types.RemediationParameterValue
import Network.AWS.Config.Types.RemediationTargetType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that represents the details about the remediation configuration that includes the remediation action, parameters, and data to execute the action.
--
--
--
-- /See:/ 'remediationConfiguration' smart constructor.
data RemediationConfiguration = RemediationConfiguration'
  { _rcResourceType ::
      !(Maybe Text),
    _rcARN :: !(Maybe Text),
    _rcAutomatic :: !(Maybe Bool),
    _rcCreatedByService :: !(Maybe Text),
    _rcRetryAttemptSeconds :: !(Maybe Nat),
    _rcExecutionControls ::
      !(Maybe ExecutionControls),
    _rcParameters ::
      !( Maybe
           ( Map
               Text
               (RemediationParameterValue)
           )
       ),
    _rcMaximumAutomaticAttempts ::
      !(Maybe Nat),
    _rcTargetVersion :: !(Maybe Text),
    _rcConfigRuleName :: !Text,
    _rcTargetType :: !RemediationTargetType,
    _rcTargetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemediationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcResourceType' - The type of a resource.
--
-- * 'rcARN' - Amazon Resource Name (ARN) of remediation configuration.
--
-- * 'rcAutomatic' - The remediation is triggered automatically.
--
-- * 'rcCreatedByService' - Name of the service that owns the service linked rule, if applicable.
--
-- * 'rcRetryAttemptSeconds' - Maximum time in seconds that AWS Config runs auto-remediation. If you do not select a number, the default is 60 seconds.  For example, if you specify RetryAttemptsSeconds as 50 seconds and MaximumAutomaticAttempts as 5, AWS Config will run auto-remediations 5 times within 50 seconds before throwing an exception.
--
-- * 'rcExecutionControls' - An ExecutionControls object.
--
-- * 'rcParameters' - An object of the RemediationParameterValue.
--
-- * 'rcMaximumAutomaticAttempts' - The maximum number of failed attempts for auto-remediation. If you do not select a number, the default is 5. For example, if you specify MaximumAutomaticAttempts as 5 with RetryAttemptsSeconds as 50 seconds, AWS Config will put a RemediationException on your behalf for the failing resource after the 5th failed attempt within 50 seconds.
--
-- * 'rcTargetVersion' - Version of the target. For example, version of the SSM document.
--
-- * 'rcConfigRuleName' - The name of the AWS Config rule.
--
-- * 'rcTargetType' - The type of the target. Target executes remediation. For example, SSM document.
--
-- * 'rcTargetId' - Target ID is the name of the public document.
remediationConfiguration ::
  -- | 'rcConfigRuleName'
  Text ->
  -- | 'rcTargetType'
  RemediationTargetType ->
  -- | 'rcTargetId'
  Text ->
  RemediationConfiguration
remediationConfiguration pConfigRuleName_ pTargetType_ pTargetId_ =
  RemediationConfiguration'
    { _rcResourceType = Nothing,
      _rcARN = Nothing,
      _rcAutomatic = Nothing,
      _rcCreatedByService = Nothing,
      _rcRetryAttemptSeconds = Nothing,
      _rcExecutionControls = Nothing,
      _rcParameters = Nothing,
      _rcMaximumAutomaticAttempts = Nothing,
      _rcTargetVersion = Nothing,
      _rcConfigRuleName = pConfigRuleName_,
      _rcTargetType = pTargetType_,
      _rcTargetId = pTargetId_
    }

-- | The type of a resource.
rcResourceType :: Lens' RemediationConfiguration (Maybe Text)
rcResourceType = lens _rcResourceType (\s a -> s {_rcResourceType = a})

-- | Amazon Resource Name (ARN) of remediation configuration.
rcARN :: Lens' RemediationConfiguration (Maybe Text)
rcARN = lens _rcARN (\s a -> s {_rcARN = a})

-- | The remediation is triggered automatically.
rcAutomatic :: Lens' RemediationConfiguration (Maybe Bool)
rcAutomatic = lens _rcAutomatic (\s a -> s {_rcAutomatic = a})

-- | Name of the service that owns the service linked rule, if applicable.
rcCreatedByService :: Lens' RemediationConfiguration (Maybe Text)
rcCreatedByService = lens _rcCreatedByService (\s a -> s {_rcCreatedByService = a})

-- | Maximum time in seconds that AWS Config runs auto-remediation. If you do not select a number, the default is 60 seconds.  For example, if you specify RetryAttemptsSeconds as 50 seconds and MaximumAutomaticAttempts as 5, AWS Config will run auto-remediations 5 times within 50 seconds before throwing an exception.
rcRetryAttemptSeconds :: Lens' RemediationConfiguration (Maybe Natural)
rcRetryAttemptSeconds = lens _rcRetryAttemptSeconds (\s a -> s {_rcRetryAttemptSeconds = a}) . mapping _Nat

-- | An ExecutionControls object.
rcExecutionControls :: Lens' RemediationConfiguration (Maybe ExecutionControls)
rcExecutionControls = lens _rcExecutionControls (\s a -> s {_rcExecutionControls = a})

-- | An object of the RemediationParameterValue.
rcParameters :: Lens' RemediationConfiguration (HashMap Text (RemediationParameterValue))
rcParameters = lens _rcParameters (\s a -> s {_rcParameters = a}) . _Default . _Map

-- | The maximum number of failed attempts for auto-remediation. If you do not select a number, the default is 5. For example, if you specify MaximumAutomaticAttempts as 5 with RetryAttemptsSeconds as 50 seconds, AWS Config will put a RemediationException on your behalf for the failing resource after the 5th failed attempt within 50 seconds.
rcMaximumAutomaticAttempts :: Lens' RemediationConfiguration (Maybe Natural)
rcMaximumAutomaticAttempts = lens _rcMaximumAutomaticAttempts (\s a -> s {_rcMaximumAutomaticAttempts = a}) . mapping _Nat

-- | Version of the target. For example, version of the SSM document.
rcTargetVersion :: Lens' RemediationConfiguration (Maybe Text)
rcTargetVersion = lens _rcTargetVersion (\s a -> s {_rcTargetVersion = a})

-- | The name of the AWS Config rule.
rcConfigRuleName :: Lens' RemediationConfiguration Text
rcConfigRuleName = lens _rcConfigRuleName (\s a -> s {_rcConfigRuleName = a})

-- | The type of the target. Target executes remediation. For example, SSM document.
rcTargetType :: Lens' RemediationConfiguration RemediationTargetType
rcTargetType = lens _rcTargetType (\s a -> s {_rcTargetType = a})

-- | Target ID is the name of the public document.
rcTargetId :: Lens' RemediationConfiguration Text
rcTargetId = lens _rcTargetId (\s a -> s {_rcTargetId = a})

instance FromJSON RemediationConfiguration where
  parseJSON =
    withObject
      "RemediationConfiguration"
      ( \x ->
          RemediationConfiguration'
            <$> (x .:? "ResourceType")
            <*> (x .:? "Arn")
            <*> (x .:? "Automatic")
            <*> (x .:? "CreatedByService")
            <*> (x .:? "RetryAttemptSeconds")
            <*> (x .:? "ExecutionControls")
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "MaximumAutomaticAttempts")
            <*> (x .:? "TargetVersion")
            <*> (x .: "ConfigRuleName")
            <*> (x .: "TargetType")
            <*> (x .: "TargetId")
      )

instance Hashable RemediationConfiguration

instance NFData RemediationConfiguration

instance ToJSON RemediationConfiguration where
  toJSON RemediationConfiguration' {..} =
    object
      ( catMaybes
          [ ("ResourceType" .=) <$> _rcResourceType,
            ("Arn" .=) <$> _rcARN,
            ("Automatic" .=) <$> _rcAutomatic,
            ("CreatedByService" .=) <$> _rcCreatedByService,
            ("RetryAttemptSeconds" .=) <$> _rcRetryAttemptSeconds,
            ("ExecutionControls" .=) <$> _rcExecutionControls,
            ("Parameters" .=) <$> _rcParameters,
            ("MaximumAutomaticAttempts" .=) <$> _rcMaximumAutomaticAttempts,
            ("TargetVersion" .=) <$> _rcTargetVersion,
            Just ("ConfigRuleName" .= _rcConfigRuleName),
            Just ("TargetType" .= _rcTargetType),
            Just ("TargetId" .= _rcTargetId)
          ]
      )
