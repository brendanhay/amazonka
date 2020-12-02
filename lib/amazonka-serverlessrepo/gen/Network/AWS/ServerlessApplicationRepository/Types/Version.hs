{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Version
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.Version where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServerlessApplicationRepository.Types.Capability
import Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition

-- | Application version details.
--
--
--
-- /See:/ 'version' smart constructor.
data Version = Version'
  { _vSourceCodeURL :: !(Maybe Text),
    _vSourceCodeArchiveURL :: !(Maybe Text),
    _vTemplateURL :: !Text,
    _vParameterDefinitions :: ![ParameterDefinition],
    _vResourcesSupported :: !Bool,
    _vCreationTime :: !Text,
    _vRequiredCapabilities :: ![Capability],
    _vApplicationId :: !Text,
    _vSemanticVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Version' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vSourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- * 'vSourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
--
-- * 'vTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'vParameterDefinitions' - An array of parameter types supported by the application.
--
-- * 'vResourcesSupported' - Whether all of the AWS resources contained in this application are supported in the region  in which it is being retrieved.
--
-- * 'vCreationTime' - The date and time this resource was created.
--
-- * 'vRequiredCapabilities' - A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
--
-- * 'vApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'vSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
version ::
  -- | 'vTemplateURL'
  Text ->
  -- | 'vResourcesSupported'
  Bool ->
  -- | 'vCreationTime'
  Text ->
  -- | 'vApplicationId'
  Text ->
  -- | 'vSemanticVersion'
  Text ->
  Version
version
  pTemplateURL_
  pResourcesSupported_
  pCreationTime_
  pApplicationId_
  pSemanticVersion_ =
    Version'
      { _vSourceCodeURL = Nothing,
        _vSourceCodeArchiveURL = Nothing,
        _vTemplateURL = pTemplateURL_,
        _vParameterDefinitions = mempty,
        _vResourcesSupported = pResourcesSupported_,
        _vCreationTime = pCreationTime_,
        _vRequiredCapabilities = mempty,
        _vApplicationId = pApplicationId_,
        _vSemanticVersion = pSemanticVersion_
      }

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
vSourceCodeURL :: Lens' Version (Maybe Text)
vSourceCodeURL = lens _vSourceCodeURL (\s a -> s {_vSourceCodeURL = a})

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
vSourceCodeArchiveURL :: Lens' Version (Maybe Text)
vSourceCodeArchiveURL = lens _vSourceCodeArchiveURL (\s a -> s {_vSourceCodeArchiveURL = a})

-- | A link to the packaged AWS SAM template of your application.
vTemplateURL :: Lens' Version Text
vTemplateURL = lens _vTemplateURL (\s a -> s {_vTemplateURL = a})

-- | An array of parameter types supported by the application.
vParameterDefinitions :: Lens' Version [ParameterDefinition]
vParameterDefinitions = lens _vParameterDefinitions (\s a -> s {_vParameterDefinitions = a}) . _Coerce

-- | Whether all of the AWS resources contained in this application are supported in the region  in which it is being retrieved.
vResourcesSupported :: Lens' Version Bool
vResourcesSupported = lens _vResourcesSupported (\s a -> s {_vResourcesSupported = a})

-- | The date and time this resource was created.
vCreationTime :: Lens' Version Text
vCreationTime = lens _vCreationTime (\s a -> s {_vCreationTime = a})

-- | A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
vRequiredCapabilities :: Lens' Version [Capability]
vRequiredCapabilities = lens _vRequiredCapabilities (\s a -> s {_vRequiredCapabilities = a}) . _Coerce

-- | The application Amazon Resource Name (ARN).
vApplicationId :: Lens' Version Text
vApplicationId = lens _vApplicationId (\s a -> s {_vApplicationId = a})

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
vSemanticVersion :: Lens' Version Text
vSemanticVersion = lens _vSemanticVersion (\s a -> s {_vSemanticVersion = a})

instance FromJSON Version where
  parseJSON =
    withObject
      "Version"
      ( \x ->
          Version'
            <$> (x .:? "sourceCodeUrl")
            <*> (x .:? "sourceCodeArchiveUrl")
            <*> (x .: "templateUrl")
            <*> (x .:? "parameterDefinitions" .!= mempty)
            <*> (x .: "resourcesSupported")
            <*> (x .: "creationTime")
            <*> (x .:? "requiredCapabilities" .!= mempty)
            <*> (x .: "applicationId")
            <*> (x .: "semanticVersion")
      )

instance Hashable Version

instance NFData Version
