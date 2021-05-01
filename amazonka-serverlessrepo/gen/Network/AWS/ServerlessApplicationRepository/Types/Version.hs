{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Version
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.Version where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServerlessApplicationRepository.Types.Capability
import Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition

-- | Application version details.
--
-- /See:/ 'newVersion' smart constructor.
data Version = Version'
  { -- | A link to the S3 object that contains the ZIP archive of the source code
    -- for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to a public repository for the source code of your application,
    -- for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to the packaged AWS SAM template of your application.
    templateUrl :: Prelude.Text,
    -- | An array of parameter types supported by the application.
    parameterDefinitions :: [ParameterDefinition],
    -- | Whether all of the AWS resources contained in this application are
    -- supported in the region in which it is being retrieved.
    resourcesSupported :: Prelude.Bool,
    -- | The date and time this resource was created.
    creationTime :: Prelude.Text,
    -- | A list of values that you must specify before you can deploy certain
    -- applications. Some applications might include resources that can affect
    -- permissions in your AWS account, for example, by creating new AWS
    -- Identity and Access Management (IAM) users. For those applications, you
    -- must explicitly acknowledge their capabilities by specifying this
    -- parameter.
    --
    -- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
    -- CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
    --
    -- The following resources require you to specify CAPABILITY_IAM or
    -- CAPABILITY_NAMED_IAM:
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy>,
    -- and
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>.
    -- If the application contains IAM resources, you can specify either
    -- CAPABILITY_IAM or CAPABILITY_NAMED_IAM. If the application contains IAM
    -- resources with custom names, you must specify CAPABILITY_NAMED_IAM.
    --
    -- The following resources require you to specify
    -- CAPABILITY_RESOURCE_POLICY:
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy>,
    -- and
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy>.
    --
    -- Applications that contain one or more nested applications require you to
    -- specify CAPABILITY_AUTO_EXPAND.
    --
    -- If your application template contains any of the above resources, we
    -- recommend that you review all permissions associated with the
    -- application before deploying. If you don\'t specify this parameter for
    -- an application that requires capabilities, the call will fail.
    requiredCapabilities :: [Capability],
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Version' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceCodeArchiveUrl', 'version_sourceCodeArchiveUrl' - A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
--
-- 'sourceCodeUrl', 'version_sourceCodeUrl' - A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
--
-- 'templateUrl', 'version_templateUrl' - A link to the packaged AWS SAM template of your application.
--
-- 'parameterDefinitions', 'version_parameterDefinitions' - An array of parameter types supported by the application.
--
-- 'resourcesSupported', 'version_resourcesSupported' - Whether all of the AWS resources contained in this application are
-- supported in the region in which it is being retrieved.
--
-- 'creationTime', 'version_creationTime' - The date and time this resource was created.
--
-- 'requiredCapabilities', 'version_requiredCapabilities' - A list of values that you must specify before you can deploy certain
-- applications. Some applications might include resources that can affect
-- permissions in your AWS account, for example, by creating new AWS
-- Identity and Access Management (IAM) users. For those applications, you
-- must explicitly acknowledge their capabilities by specifying this
-- parameter.
--
-- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
-- CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
--
-- The following resources require you to specify CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>.
-- If the application contains IAM resources, you can specify either
-- CAPABILITY_IAM or CAPABILITY_NAMED_IAM. If the application contains IAM
-- resources with custom names, you must specify CAPABILITY_NAMED_IAM.
--
-- The following resources require you to specify
-- CAPABILITY_RESOURCE_POLICY:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy>.
--
-- Applications that contain one or more nested applications require you to
-- specify CAPABILITY_AUTO_EXPAND.
--
-- If your application template contains any of the above resources, we
-- recommend that you review all permissions associated with the
-- application before deploying. If you don\'t specify this parameter for
-- an application that requires capabilities, the call will fail.
--
-- 'applicationId', 'version_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'semanticVersion', 'version_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
newVersion ::
  -- | 'templateUrl'
  Prelude.Text ->
  -- | 'resourcesSupported'
  Prelude.Bool ->
  -- | 'creationTime'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  Version
newVersion
  pTemplateUrl_
  pResourcesSupported_
  pCreationTime_
  pApplicationId_
  pSemanticVersion_ =
    Version'
      { sourceCodeArchiveUrl = Prelude.Nothing,
        sourceCodeUrl = Prelude.Nothing,
        templateUrl = pTemplateUrl_,
        parameterDefinitions = Prelude.mempty,
        resourcesSupported = pResourcesSupported_,
        creationTime = pCreationTime_,
        requiredCapabilities = Prelude.mempty,
        applicationId = pApplicationId_,
        semanticVersion = pSemanticVersion_
      }

-- | A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
version_sourceCodeArchiveUrl :: Lens.Lens' Version (Prelude.Maybe Prelude.Text)
version_sourceCodeArchiveUrl = Lens.lens (\Version' {sourceCodeArchiveUrl} -> sourceCodeArchiveUrl) (\s@Version' {} a -> s {sourceCodeArchiveUrl = a} :: Version)

-- | A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
version_sourceCodeUrl :: Lens.Lens' Version (Prelude.Maybe Prelude.Text)
version_sourceCodeUrl = Lens.lens (\Version' {sourceCodeUrl} -> sourceCodeUrl) (\s@Version' {} a -> s {sourceCodeUrl = a} :: Version)

-- | A link to the packaged AWS SAM template of your application.
version_templateUrl :: Lens.Lens' Version Prelude.Text
version_templateUrl = Lens.lens (\Version' {templateUrl} -> templateUrl) (\s@Version' {} a -> s {templateUrl = a} :: Version)

-- | An array of parameter types supported by the application.
version_parameterDefinitions :: Lens.Lens' Version [ParameterDefinition]
version_parameterDefinitions = Lens.lens (\Version' {parameterDefinitions} -> parameterDefinitions) (\s@Version' {} a -> s {parameterDefinitions = a} :: Version) Prelude.. Prelude._Coerce

-- | Whether all of the AWS resources contained in this application are
-- supported in the region in which it is being retrieved.
version_resourcesSupported :: Lens.Lens' Version Prelude.Bool
version_resourcesSupported = Lens.lens (\Version' {resourcesSupported} -> resourcesSupported) (\s@Version' {} a -> s {resourcesSupported = a} :: Version)

-- | The date and time this resource was created.
version_creationTime :: Lens.Lens' Version Prelude.Text
version_creationTime = Lens.lens (\Version' {creationTime} -> creationTime) (\s@Version' {} a -> s {creationTime = a} :: Version)

-- | A list of values that you must specify before you can deploy certain
-- applications. Some applications might include resources that can affect
-- permissions in your AWS account, for example, by creating new AWS
-- Identity and Access Management (IAM) users. For those applications, you
-- must explicitly acknowledge their capabilities by specifying this
-- parameter.
--
-- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
-- CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
--
-- The following resources require you to specify CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>.
-- If the application contains IAM resources, you can specify either
-- CAPABILITY_IAM or CAPABILITY_NAMED_IAM. If the application contains IAM
-- resources with custom names, you must specify CAPABILITY_NAMED_IAM.
--
-- The following resources require you to specify
-- CAPABILITY_RESOURCE_POLICY:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy>.
--
-- Applications that contain one or more nested applications require you to
-- specify CAPABILITY_AUTO_EXPAND.
--
-- If your application template contains any of the above resources, we
-- recommend that you review all permissions associated with the
-- application before deploying. If you don\'t specify this parameter for
-- an application that requires capabilities, the call will fail.
version_requiredCapabilities :: Lens.Lens' Version [Capability]
version_requiredCapabilities = Lens.lens (\Version' {requiredCapabilities} -> requiredCapabilities) (\s@Version' {} a -> s {requiredCapabilities = a} :: Version) Prelude.. Prelude._Coerce

-- | The application Amazon Resource Name (ARN).
version_applicationId :: Lens.Lens' Version Prelude.Text
version_applicationId = Lens.lens (\Version' {applicationId} -> applicationId) (\s@Version' {} a -> s {applicationId = a} :: Version)

-- | The semantic version of the application:
--
-- <https://semver.org/>
version_semanticVersion :: Lens.Lens' Version Prelude.Text
version_semanticVersion = Lens.lens (\Version' {semanticVersion} -> semanticVersion) (\s@Version' {} a -> s {semanticVersion = a} :: Version)

instance Prelude.FromJSON Version where
  parseJSON =
    Prelude.withObject
      "Version"
      ( \x ->
          Version'
            Prelude.<$> (x Prelude..:? "sourceCodeArchiveUrl")
            Prelude.<*> (x Prelude..:? "sourceCodeUrl")
            Prelude.<*> (x Prelude..: "templateUrl")
            Prelude.<*> ( x Prelude..:? "parameterDefinitions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "resourcesSupported")
            Prelude.<*> (x Prelude..: "creationTime")
            Prelude.<*> ( x Prelude..:? "requiredCapabilities"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "applicationId")
            Prelude.<*> (x Prelude..: "semanticVersion")
      )

instance Prelude.Hashable Version

instance Prelude.NFData Version
