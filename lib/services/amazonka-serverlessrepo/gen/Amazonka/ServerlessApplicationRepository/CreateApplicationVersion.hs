{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServerlessApplicationRepository.CreateApplicationVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version.
module Amazonka.ServerlessApplicationRepository.CreateApplicationVersion
  ( -- * Creating a Request
    CreateApplicationVersion (..),
    newCreateApplicationVersion,

    -- * Request Lenses
    createApplicationVersion_sourceCodeArchiveUrl,
    createApplicationVersion_sourceCodeUrl,
    createApplicationVersion_templateBody,
    createApplicationVersion_templateUrl,
    createApplicationVersion_applicationId,
    createApplicationVersion_semanticVersion,

    -- * Destructuring the Response
    CreateApplicationVersionResponse (..),
    newCreateApplicationVersionResponse,

    -- * Response Lenses
    createApplicationVersionResponse_applicationId,
    createApplicationVersionResponse_creationTime,
    createApplicationVersionResponse_parameterDefinitions,
    createApplicationVersionResponse_requiredCapabilities,
    createApplicationVersionResponse_resourcesSupported,
    createApplicationVersionResponse_semanticVersion,
    createApplicationVersionResponse_sourceCodeArchiveUrl,
    createApplicationVersionResponse_sourceCodeUrl,
    createApplicationVersionResponse_templateUrl,
    createApplicationVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newCreateApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { -- | A link to the S3 object that contains the ZIP archive of the source code
    -- for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to a public repository for the source code of your application,
    -- for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Prelude.Maybe Prelude.Text,
    -- | The raw packaged AWS SAM template of your application.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | A link to the packaged AWS SAM template of your application.
    templateUrl :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text,
    -- | The semantic version of the new version.
    semanticVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceCodeArchiveUrl', 'createApplicationVersion_sourceCodeArchiveUrl' - A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
--
-- 'sourceCodeUrl', 'createApplicationVersion_sourceCodeUrl' - A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
--
-- 'templateBody', 'createApplicationVersion_templateBody' - The raw packaged AWS SAM template of your application.
--
-- 'templateUrl', 'createApplicationVersion_templateUrl' - A link to the packaged AWS SAM template of your application.
--
-- 'applicationId', 'createApplicationVersion_applicationId' - The Amazon Resource Name (ARN) of the application.
--
-- 'semanticVersion', 'createApplicationVersion_semanticVersion' - The semantic version of the new version.
newCreateApplicationVersion ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  CreateApplicationVersion
newCreateApplicationVersion
  pApplicationId_
  pSemanticVersion_ =
    CreateApplicationVersion'
      { sourceCodeArchiveUrl =
          Prelude.Nothing,
        sourceCodeUrl = Prelude.Nothing,
        templateBody = Prelude.Nothing,
        templateUrl = Prelude.Nothing,
        applicationId = pApplicationId_,
        semanticVersion = pSemanticVersion_
      }

-- | A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
createApplicationVersion_sourceCodeArchiveUrl :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe Prelude.Text)
createApplicationVersion_sourceCodeArchiveUrl = Lens.lens (\CreateApplicationVersion' {sourceCodeArchiveUrl} -> sourceCodeArchiveUrl) (\s@CreateApplicationVersion' {} a -> s {sourceCodeArchiveUrl = a} :: CreateApplicationVersion)

-- | A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
createApplicationVersion_sourceCodeUrl :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe Prelude.Text)
createApplicationVersion_sourceCodeUrl = Lens.lens (\CreateApplicationVersion' {sourceCodeUrl} -> sourceCodeUrl) (\s@CreateApplicationVersion' {} a -> s {sourceCodeUrl = a} :: CreateApplicationVersion)

-- | The raw packaged AWS SAM template of your application.
createApplicationVersion_templateBody :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe Prelude.Text)
createApplicationVersion_templateBody = Lens.lens (\CreateApplicationVersion' {templateBody} -> templateBody) (\s@CreateApplicationVersion' {} a -> s {templateBody = a} :: CreateApplicationVersion)

-- | A link to the packaged AWS SAM template of your application.
createApplicationVersion_templateUrl :: Lens.Lens' CreateApplicationVersion (Prelude.Maybe Prelude.Text)
createApplicationVersion_templateUrl = Lens.lens (\CreateApplicationVersion' {templateUrl} -> templateUrl) (\s@CreateApplicationVersion' {} a -> s {templateUrl = a} :: CreateApplicationVersion)

-- | The Amazon Resource Name (ARN) of the application.
createApplicationVersion_applicationId :: Lens.Lens' CreateApplicationVersion Prelude.Text
createApplicationVersion_applicationId = Lens.lens (\CreateApplicationVersion' {applicationId} -> applicationId) (\s@CreateApplicationVersion' {} a -> s {applicationId = a} :: CreateApplicationVersion)

-- | The semantic version of the new version.
createApplicationVersion_semanticVersion :: Lens.Lens' CreateApplicationVersion Prelude.Text
createApplicationVersion_semanticVersion = Lens.lens (\CreateApplicationVersion' {semanticVersion} -> semanticVersion) (\s@CreateApplicationVersion' {} a -> s {semanticVersion = a} :: CreateApplicationVersion)

instance Core.AWSRequest CreateApplicationVersion where
  type
    AWSResponse CreateApplicationVersion =
      CreateApplicationVersionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationVersionResponse'
            Prelude.<$> (x Data..?> "applicationId")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> ( x Data..?> "parameterDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "requiredCapabilities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "resourcesSupported")
            Prelude.<*> (x Data..?> "semanticVersion")
            Prelude.<*> (x Data..?> "sourceCodeArchiveUrl")
            Prelude.<*> (x Data..?> "sourceCodeUrl")
            Prelude.<*> (x Data..?> "templateUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplicationVersion where
  hashWithSalt _salt CreateApplicationVersion' {..} =
    _salt `Prelude.hashWithSalt` sourceCodeArchiveUrl
      `Prelude.hashWithSalt` sourceCodeUrl
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` templateUrl
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` semanticVersion

instance Prelude.NFData CreateApplicationVersion where
  rnf CreateApplicationVersion' {..} =
    Prelude.rnf sourceCodeArchiveUrl
      `Prelude.seq` Prelude.rnf sourceCodeUrl
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateUrl
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf semanticVersion

instance Data.ToHeaders CreateApplicationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplicationVersion where
  toJSON CreateApplicationVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sourceCodeArchiveUrl" Data..=)
              Prelude.<$> sourceCodeArchiveUrl,
            ("sourceCodeUrl" Data..=) Prelude.<$> sourceCodeUrl,
            ("templateBody" Data..=) Prelude.<$> templateBody,
            ("templateUrl" Data..=) Prelude.<$> templateUrl
          ]
      )

instance Data.ToPath CreateApplicationVersion where
  toPath CreateApplicationVersion' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/versions/",
        Data.toBS semanticVersion
      ]

instance Data.ToQuery CreateApplicationVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationVersionResponse' smart constructor.
data CreateApplicationVersionResponse = CreateApplicationVersionResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time this resource was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | An array of parameter types supported by the application.
    parameterDefinitions :: Prelude.Maybe [ParameterDefinition],
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
    requiredCapabilities :: Prelude.Maybe [Capability],
    -- | Whether all of the AWS resources contained in this application are
    -- supported in the region in which it is being retrieved.
    resourcesSupported :: Prelude.Maybe Prelude.Bool,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | A link to the S3 object that contains the ZIP archive of the source code
    -- for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to a public repository for the source code of your application,
    -- for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to the packaged AWS SAM template of your application.
    templateUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createApplicationVersionResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'creationTime', 'createApplicationVersionResponse_creationTime' - The date and time this resource was created.
--
-- 'parameterDefinitions', 'createApplicationVersionResponse_parameterDefinitions' - An array of parameter types supported by the application.
--
-- 'requiredCapabilities', 'createApplicationVersionResponse_requiredCapabilities' - A list of values that you must specify before you can deploy certain
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
-- 'resourcesSupported', 'createApplicationVersionResponse_resourcesSupported' - Whether all of the AWS resources contained in this application are
-- supported in the region in which it is being retrieved.
--
-- 'semanticVersion', 'createApplicationVersionResponse_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'sourceCodeArchiveUrl', 'createApplicationVersionResponse_sourceCodeArchiveUrl' - A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
--
-- 'sourceCodeUrl', 'createApplicationVersionResponse_sourceCodeUrl' - A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
--
-- 'templateUrl', 'createApplicationVersionResponse_templateUrl' - A link to the packaged AWS SAM template of your application.
--
-- 'httpStatus', 'createApplicationVersionResponse_httpStatus' - The response's http status code.
newCreateApplicationVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationVersionResponse
newCreateApplicationVersionResponse pHttpStatus_ =
  CreateApplicationVersionResponse'
    { applicationId =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      parameterDefinitions = Prelude.Nothing,
      requiredCapabilities = Prelude.Nothing,
      resourcesSupported = Prelude.Nothing,
      semanticVersion = Prelude.Nothing,
      sourceCodeArchiveUrl = Prelude.Nothing,
      sourceCodeUrl = Prelude.Nothing,
      templateUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
createApplicationVersionResponse_applicationId :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createApplicationVersionResponse_applicationId = Lens.lens (\CreateApplicationVersionResponse' {applicationId} -> applicationId) (\s@CreateApplicationVersionResponse' {} a -> s {applicationId = a} :: CreateApplicationVersionResponse)

-- | The date and time this resource was created.
createApplicationVersionResponse_creationTime :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createApplicationVersionResponse_creationTime = Lens.lens (\CreateApplicationVersionResponse' {creationTime} -> creationTime) (\s@CreateApplicationVersionResponse' {} a -> s {creationTime = a} :: CreateApplicationVersionResponse)

-- | An array of parameter types supported by the application.
createApplicationVersionResponse_parameterDefinitions :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe [ParameterDefinition])
createApplicationVersionResponse_parameterDefinitions = Lens.lens (\CreateApplicationVersionResponse' {parameterDefinitions} -> parameterDefinitions) (\s@CreateApplicationVersionResponse' {} a -> s {parameterDefinitions = a} :: CreateApplicationVersionResponse) Prelude.. Lens.mapping Lens.coerced

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
createApplicationVersionResponse_requiredCapabilities :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe [Capability])
createApplicationVersionResponse_requiredCapabilities = Lens.lens (\CreateApplicationVersionResponse' {requiredCapabilities} -> requiredCapabilities) (\s@CreateApplicationVersionResponse' {} a -> s {requiredCapabilities = a} :: CreateApplicationVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Whether all of the AWS resources contained in this application are
-- supported in the region in which it is being retrieved.
createApplicationVersionResponse_resourcesSupported :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe Prelude.Bool)
createApplicationVersionResponse_resourcesSupported = Lens.lens (\CreateApplicationVersionResponse' {resourcesSupported} -> resourcesSupported) (\s@CreateApplicationVersionResponse' {} a -> s {resourcesSupported = a} :: CreateApplicationVersionResponse)

-- | The semantic version of the application:
--
-- <https://semver.org/>
createApplicationVersionResponse_semanticVersion :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createApplicationVersionResponse_semanticVersion = Lens.lens (\CreateApplicationVersionResponse' {semanticVersion} -> semanticVersion) (\s@CreateApplicationVersionResponse' {} a -> s {semanticVersion = a} :: CreateApplicationVersionResponse)

-- | A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
createApplicationVersionResponse_sourceCodeArchiveUrl :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createApplicationVersionResponse_sourceCodeArchiveUrl = Lens.lens (\CreateApplicationVersionResponse' {sourceCodeArchiveUrl} -> sourceCodeArchiveUrl) (\s@CreateApplicationVersionResponse' {} a -> s {sourceCodeArchiveUrl = a} :: CreateApplicationVersionResponse)

-- | A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
createApplicationVersionResponse_sourceCodeUrl :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createApplicationVersionResponse_sourceCodeUrl = Lens.lens (\CreateApplicationVersionResponse' {sourceCodeUrl} -> sourceCodeUrl) (\s@CreateApplicationVersionResponse' {} a -> s {sourceCodeUrl = a} :: CreateApplicationVersionResponse)

-- | A link to the packaged AWS SAM template of your application.
createApplicationVersionResponse_templateUrl :: Lens.Lens' CreateApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createApplicationVersionResponse_templateUrl = Lens.lens (\CreateApplicationVersionResponse' {templateUrl} -> templateUrl) (\s@CreateApplicationVersionResponse' {} a -> s {templateUrl = a} :: CreateApplicationVersionResponse)

-- | The response's http status code.
createApplicationVersionResponse_httpStatus :: Lens.Lens' CreateApplicationVersionResponse Prelude.Int
createApplicationVersionResponse_httpStatus = Lens.lens (\CreateApplicationVersionResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationVersionResponse' {} a -> s {httpStatus = a} :: CreateApplicationVersionResponse)

instance
  Prelude.NFData
    CreateApplicationVersionResponse
  where
  rnf CreateApplicationVersionResponse' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf parameterDefinitions
      `Prelude.seq` Prelude.rnf requiredCapabilities
      `Prelude.seq` Prelude.rnf resourcesSupported
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf sourceCodeArchiveUrl
      `Prelude.seq` Prelude.rnf sourceCodeUrl
      `Prelude.seq` Prelude.rnf templateUrl
      `Prelude.seq` Prelude.rnf httpStatus
