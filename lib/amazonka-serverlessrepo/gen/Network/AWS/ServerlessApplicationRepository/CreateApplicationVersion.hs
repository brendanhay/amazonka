{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version.
module Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion
  ( -- * Creating a Request
    createApplicationVersion,
    CreateApplicationVersion,

    -- * Request Lenses
    cavSourceCodeURL,
    cavTemplateBody,
    cavTemplateURL,
    cavSourceCodeArchiveURL,
    cavApplicationId,
    cavSemanticVersion,

    -- * Destructuring the Response
    createApplicationVersionResponse,
    CreateApplicationVersionResponse,

    -- * Response Lenses
    cavrsCreationTime,
    cavrsResourcesSupported,
    cavrsRequiredCapabilities,
    cavrsParameterDefinitions,
    cavrsSemanticVersion,
    cavrsSourceCodeURL,
    cavrsApplicationId,
    cavrsTemplateURL,
    cavrsSourceCodeArchiveURL,
    cavrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'createApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { _cavSourceCodeURL ::
      !(Maybe Text),
    _cavTemplateBody :: !(Maybe Text),
    _cavTemplateURL :: !(Maybe Text),
    _cavSourceCodeArchiveURL :: !(Maybe Text),
    _cavApplicationId :: !Text,
    _cavSemanticVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateApplicationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cavSourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- * 'cavTemplateBody' - The raw packaged AWS SAM template of your application.
--
-- * 'cavTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'cavSourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
--
-- * 'cavApplicationId' - The Amazon Resource Name (ARN) of the application.
--
-- * 'cavSemanticVersion' - The semantic version of the new version.
createApplicationVersion ::
  -- | 'cavApplicationId'
  Text ->
  -- | 'cavSemanticVersion'
  Text ->
  CreateApplicationVersion
createApplicationVersion pApplicationId_ pSemanticVersion_ =
  CreateApplicationVersion'
    { _cavSourceCodeURL = Nothing,
      _cavTemplateBody = Nothing,
      _cavTemplateURL = Nothing,
      _cavSourceCodeArchiveURL = Nothing,
      _cavApplicationId = pApplicationId_,
      _cavSemanticVersion = pSemanticVersion_
    }

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
cavSourceCodeURL :: Lens' CreateApplicationVersion (Maybe Text)
cavSourceCodeURL = lens _cavSourceCodeURL (\s a -> s {_cavSourceCodeURL = a})

-- | The raw packaged AWS SAM template of your application.
cavTemplateBody :: Lens' CreateApplicationVersion (Maybe Text)
cavTemplateBody = lens _cavTemplateBody (\s a -> s {_cavTemplateBody = a})

-- | A link to the packaged AWS SAM template of your application.
cavTemplateURL :: Lens' CreateApplicationVersion (Maybe Text)
cavTemplateURL = lens _cavTemplateURL (\s a -> s {_cavTemplateURL = a})

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
cavSourceCodeArchiveURL :: Lens' CreateApplicationVersion (Maybe Text)
cavSourceCodeArchiveURL = lens _cavSourceCodeArchiveURL (\s a -> s {_cavSourceCodeArchiveURL = a})

-- | The Amazon Resource Name (ARN) of the application.
cavApplicationId :: Lens' CreateApplicationVersion Text
cavApplicationId = lens _cavApplicationId (\s a -> s {_cavApplicationId = a})

-- | The semantic version of the new version.
cavSemanticVersion :: Lens' CreateApplicationVersion Text
cavSemanticVersion = lens _cavSemanticVersion (\s a -> s {_cavSemanticVersion = a})

instance AWSRequest CreateApplicationVersion where
  type Rs CreateApplicationVersion = CreateApplicationVersionResponse
  request = putJSON serverlessApplicationRepository
  response =
    receiveJSON
      ( \s h x ->
          CreateApplicationVersionResponse'
            <$> (x .?> "creationTime")
            <*> (x .?> "resourcesSupported")
            <*> (x .?> "requiredCapabilities" .!@ mempty)
            <*> (x .?> "parameterDefinitions" .!@ mempty)
            <*> (x .?> "semanticVersion")
            <*> (x .?> "sourceCodeUrl")
            <*> (x .?> "applicationId")
            <*> (x .?> "templateUrl")
            <*> (x .?> "sourceCodeArchiveUrl")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateApplicationVersion

instance NFData CreateApplicationVersion

instance ToHeaders CreateApplicationVersion where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateApplicationVersion where
  toJSON CreateApplicationVersion' {..} =
    object
      ( catMaybes
          [ ("sourceCodeUrl" .=) <$> _cavSourceCodeURL,
            ("templateBody" .=) <$> _cavTemplateBody,
            ("templateUrl" .=) <$> _cavTemplateURL,
            ("sourceCodeArchiveUrl" .=) <$> _cavSourceCodeArchiveURL
          ]
      )

instance ToPath CreateApplicationVersion where
  toPath CreateApplicationVersion' {..} =
    mconcat
      [ "/applications/",
        toBS _cavApplicationId,
        "/versions/",
        toBS _cavSemanticVersion
      ]

instance ToQuery CreateApplicationVersion where
  toQuery = const mempty

-- | /See:/ 'createApplicationVersionResponse' smart constructor.
data CreateApplicationVersionResponse = CreateApplicationVersionResponse'
  { _cavrsCreationTime ::
      !(Maybe Text),
    _cavrsResourcesSupported ::
      !(Maybe Bool),
    _cavrsRequiredCapabilities ::
      !(Maybe [Capability]),
    _cavrsParameterDefinitions ::
      !( Maybe
           [ParameterDefinition]
       ),
    _cavrsSemanticVersion ::
      !(Maybe Text),
    _cavrsSourceCodeURL ::
      !(Maybe Text),
    _cavrsApplicationId ::
      !(Maybe Text),
    _cavrsTemplateURL ::
      !(Maybe Text),
    _cavrsSourceCodeArchiveURL ::
      !(Maybe Text),
    _cavrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateApplicationVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cavrsCreationTime' - The date and time this resource was created.
--
-- * 'cavrsResourcesSupported' - Whether all of the AWS resources contained in this application are supported in the region  in which it is being retrieved.
--
-- * 'cavrsRequiredCapabilities' - A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
--
-- * 'cavrsParameterDefinitions' - An array of parameter types supported by the application.
--
-- * 'cavrsSemanticVersion' - The semantic version of the application: <https://semver.org/ https://semver.org/>
--
-- * 'cavrsSourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- * 'cavrsApplicationId' - The application Amazon Resource Name (ARN).
--
-- * 'cavrsTemplateURL' - A link to the packaged AWS SAM template of your application.
--
-- * 'cavrsSourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
--
-- * 'cavrsResponseStatus' - -- | The response status code.
createApplicationVersionResponse ::
  -- | 'cavrsResponseStatus'
  Int ->
  CreateApplicationVersionResponse
createApplicationVersionResponse pResponseStatus_ =
  CreateApplicationVersionResponse'
    { _cavrsCreationTime = Nothing,
      _cavrsResourcesSupported = Nothing,
      _cavrsRequiredCapabilities = Nothing,
      _cavrsParameterDefinitions = Nothing,
      _cavrsSemanticVersion = Nothing,
      _cavrsSourceCodeURL = Nothing,
      _cavrsApplicationId = Nothing,
      _cavrsTemplateURL = Nothing,
      _cavrsSourceCodeArchiveURL = Nothing,
      _cavrsResponseStatus = pResponseStatus_
    }

-- | The date and time this resource was created.
cavrsCreationTime :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsCreationTime = lens _cavrsCreationTime (\s a -> s {_cavrsCreationTime = a})

-- | Whether all of the AWS resources contained in this application are supported in the region  in which it is being retrieved.
cavrsResourcesSupported :: Lens' CreateApplicationVersionResponse (Maybe Bool)
cavrsResourcesSupported = lens _cavrsResourcesSupported (\s a -> s {_cavrsResourcesSupported = a})

-- | A list of values that you must specify before you can deploy certain applications.  Some applications might include resources that can affect permissions in your AWS  account, for example, by creating new AWS Identity and Access Management (IAM) users.  For those applications, you must explicitly acknowledge their capabilities by  specifying this parameter. The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND. The following resources require you to specify CAPABILITY_IAM or  CAPABILITY_NAMED_IAM:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .  If the application contains IAM resources, you can specify either CAPABILITY_IAM  or CAPABILITY_NAMED_IAM. If the application contains IAM resources  with custom names, you must specify CAPABILITY_NAMED_IAM. The following resources require you to specify CAPABILITY_RESOURCE_POLICY:  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> . Applications that contain one or more nested applications require you to specify  CAPABILITY_AUTO_EXPAND. If your application template contains any of the above resources, we recommend that you review  all permissions associated with the application before deploying. If you don't specify  this parameter for an application that requires capabilities, the call will fail.
cavrsRequiredCapabilities :: Lens' CreateApplicationVersionResponse [Capability]
cavrsRequiredCapabilities = lens _cavrsRequiredCapabilities (\s a -> s {_cavrsRequiredCapabilities = a}) . _Default . _Coerce

-- | An array of parameter types supported by the application.
cavrsParameterDefinitions :: Lens' CreateApplicationVersionResponse [ParameterDefinition]
cavrsParameterDefinitions = lens _cavrsParameterDefinitions (\s a -> s {_cavrsParameterDefinitions = a}) . _Default . _Coerce

-- | The semantic version of the application: <https://semver.org/ https://semver.org/>
cavrsSemanticVersion :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsSemanticVersion = lens _cavrsSemanticVersion (\s a -> s {_cavrsSemanticVersion = a})

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
cavrsSourceCodeURL :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsSourceCodeURL = lens _cavrsSourceCodeURL (\s a -> s {_cavrsSourceCodeURL = a})

-- | The application Amazon Resource Name (ARN).
cavrsApplicationId :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsApplicationId = lens _cavrsApplicationId (\s a -> s {_cavrsApplicationId = a})

-- | A link to the packaged AWS SAM template of your application.
cavrsTemplateURL :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsTemplateURL = lens _cavrsTemplateURL (\s a -> s {_cavrsTemplateURL = a})

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application. Maximum size 50 MB
cavrsSourceCodeArchiveURL :: Lens' CreateApplicationVersionResponse (Maybe Text)
cavrsSourceCodeArchiveURL = lens _cavrsSourceCodeArchiveURL (\s a -> s {_cavrsSourceCodeArchiveURL = a})

-- | -- | The response status code.
cavrsResponseStatus :: Lens' CreateApplicationVersionResponse Int
cavrsResponseStatus = lens _cavrsResponseStatus (\s a -> s {_cavrsResponseStatus = a})

instance NFData CreateApplicationVersionResponse
