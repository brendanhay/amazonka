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
-- Module      : Network.AWS.IoTThingsGraph.CreateSystemInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a system instance.
--
-- This action validates the system instance, prepares the
-- deployment-related resources. For Greengrass deployments, it updates the
-- Greengrass group that is specified by the @greengrassGroupName@
-- parameter. It also adds a file to the S3 bucket specified by the
-- @s3BucketName@ parameter. You need to call @DeploySystemInstance@ after
-- running this action.
--
-- For Greengrass deployments, since this action modifies and adds
-- resources to a Greengrass group and an S3 bucket on the caller\'s
-- behalf, the calling identity must have write permissions to both the
-- specified Greengrass group and S3 bucket. Otherwise, the call will fail
-- with an authorization error.
--
-- For cloud deployments, this action requires a @flowActionsRoleArn@
-- value. This is an IAM role that has permissions to access AWS services,
-- such as AWS Lambda and AWS IoT, that the flow uses when it executes.
--
-- If the definition document doesn\'t specify a version of the user\'s
-- namespace, the latest version will be used by default.
module Network.AWS.IoTThingsGraph.CreateSystemInstance
  ( -- * Creating a Request
    CreateSystemInstance (..),
    newCreateSystemInstance,

    -- * Request Lenses
    createSystemInstance_metricsConfiguration,
    createSystemInstance_greengrassGroupName,
    createSystemInstance_flowActionsRoleArn,
    createSystemInstance_s3BucketName,
    createSystemInstance_tags,
    createSystemInstance_definition,
    createSystemInstance_target,

    -- * Destructuring the Response
    CreateSystemInstanceResponse (..),
    newCreateSystemInstanceResponse,

    -- * Response Lenses
    createSystemInstanceResponse_summary,
    createSystemInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTThingsGraph.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSystemInstance' smart constructor.
data CreateSystemInstance = CreateSystemInstance'
  { metricsConfiguration :: Prelude.Maybe MetricsConfiguration,
    -- | The name of the Greengrass group where the system instance will be
    -- deployed. This value is required if the value of the @target@ parameter
    -- is @GREENGRASS@.
    greengrassGroupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that AWS IoT Things Graph will assume when it
    -- executes the flow. This role must have read and write access to AWS
    -- Lambda and AWS IoT and any other AWS services that the flow uses when it
    -- executes. This value is required if the value of the @target@ parameter
    -- is @CLOUD@.
    flowActionsRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Simple Storage Service bucket that will be used
    -- to store and deploy the system instance\'s resource file. This value is
    -- required if the value of the @target@ parameter is @GREENGRASS@.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Metadata, consisting of key-value pairs, that can be used to categorize
    -- your system instances.
    tags :: Prelude.Maybe [Tag],
    definition :: DefinitionDocument,
    -- | The target type of the deployment. Valid values are @GREENGRASS@ and
    -- @CLOUD@.
    target :: DeploymentTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSystemInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricsConfiguration', 'createSystemInstance_metricsConfiguration' - Undocumented member.
--
-- 'greengrassGroupName', 'createSystemInstance_greengrassGroupName' - The name of the Greengrass group where the system instance will be
-- deployed. This value is required if the value of the @target@ parameter
-- is @GREENGRASS@.
--
-- 'flowActionsRoleArn', 'createSystemInstance_flowActionsRoleArn' - The ARN of the IAM role that AWS IoT Things Graph will assume when it
-- executes the flow. This role must have read and write access to AWS
-- Lambda and AWS IoT and any other AWS services that the flow uses when it
-- executes. This value is required if the value of the @target@ parameter
-- is @CLOUD@.
--
-- 's3BucketName', 'createSystemInstance_s3BucketName' - The name of the Amazon Simple Storage Service bucket that will be used
-- to store and deploy the system instance\'s resource file. This value is
-- required if the value of the @target@ parameter is @GREENGRASS@.
--
-- 'tags', 'createSystemInstance_tags' - Metadata, consisting of key-value pairs, that can be used to categorize
-- your system instances.
--
-- 'definition', 'createSystemInstance_definition' - Undocumented member.
--
-- 'target', 'createSystemInstance_target' - The target type of the deployment. Valid values are @GREENGRASS@ and
-- @CLOUD@.
newCreateSystemInstance ::
  -- | 'definition'
  DefinitionDocument ->
  -- | 'target'
  DeploymentTarget ->
  CreateSystemInstance
newCreateSystemInstance pDefinition_ pTarget_ =
  CreateSystemInstance'
    { metricsConfiguration =
        Prelude.Nothing,
      greengrassGroupName = Prelude.Nothing,
      flowActionsRoleArn = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      tags = Prelude.Nothing,
      definition = pDefinition_,
      target = pTarget_
    }

-- | Undocumented member.
createSystemInstance_metricsConfiguration :: Lens.Lens' CreateSystemInstance (Prelude.Maybe MetricsConfiguration)
createSystemInstance_metricsConfiguration = Lens.lens (\CreateSystemInstance' {metricsConfiguration} -> metricsConfiguration) (\s@CreateSystemInstance' {} a -> s {metricsConfiguration = a} :: CreateSystemInstance)

-- | The name of the Greengrass group where the system instance will be
-- deployed. This value is required if the value of the @target@ parameter
-- is @GREENGRASS@.
createSystemInstance_greengrassGroupName :: Lens.Lens' CreateSystemInstance (Prelude.Maybe Prelude.Text)
createSystemInstance_greengrassGroupName = Lens.lens (\CreateSystemInstance' {greengrassGroupName} -> greengrassGroupName) (\s@CreateSystemInstance' {} a -> s {greengrassGroupName = a} :: CreateSystemInstance)

-- | The ARN of the IAM role that AWS IoT Things Graph will assume when it
-- executes the flow. This role must have read and write access to AWS
-- Lambda and AWS IoT and any other AWS services that the flow uses when it
-- executes. This value is required if the value of the @target@ parameter
-- is @CLOUD@.
createSystemInstance_flowActionsRoleArn :: Lens.Lens' CreateSystemInstance (Prelude.Maybe Prelude.Text)
createSystemInstance_flowActionsRoleArn = Lens.lens (\CreateSystemInstance' {flowActionsRoleArn} -> flowActionsRoleArn) (\s@CreateSystemInstance' {} a -> s {flowActionsRoleArn = a} :: CreateSystemInstance)

-- | The name of the Amazon Simple Storage Service bucket that will be used
-- to store and deploy the system instance\'s resource file. This value is
-- required if the value of the @target@ parameter is @GREENGRASS@.
createSystemInstance_s3BucketName :: Lens.Lens' CreateSystemInstance (Prelude.Maybe Prelude.Text)
createSystemInstance_s3BucketName = Lens.lens (\CreateSystemInstance' {s3BucketName} -> s3BucketName) (\s@CreateSystemInstance' {} a -> s {s3BucketName = a} :: CreateSystemInstance)

-- | Metadata, consisting of key-value pairs, that can be used to categorize
-- your system instances.
createSystemInstance_tags :: Lens.Lens' CreateSystemInstance (Prelude.Maybe [Tag])
createSystemInstance_tags = Lens.lens (\CreateSystemInstance' {tags} -> tags) (\s@CreateSystemInstance' {} a -> s {tags = a} :: CreateSystemInstance) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createSystemInstance_definition :: Lens.Lens' CreateSystemInstance DefinitionDocument
createSystemInstance_definition = Lens.lens (\CreateSystemInstance' {definition} -> definition) (\s@CreateSystemInstance' {} a -> s {definition = a} :: CreateSystemInstance)

-- | The target type of the deployment. Valid values are @GREENGRASS@ and
-- @CLOUD@.
createSystemInstance_target :: Lens.Lens' CreateSystemInstance DeploymentTarget
createSystemInstance_target = Lens.lens (\CreateSystemInstance' {target} -> target) (\s@CreateSystemInstance' {} a -> s {target = a} :: CreateSystemInstance)

instance Core.AWSRequest CreateSystemInstance where
  type
    AWSResponse CreateSystemInstance =
      CreateSystemInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSystemInstanceResponse'
            Prelude.<$> (x Core..?> "summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSystemInstance

instance Prelude.NFData CreateSystemInstance

instance Core.ToHeaders CreateSystemInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.CreateSystemInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSystemInstance where
  toJSON CreateSystemInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("metricsConfiguration" Core..=)
              Prelude.<$> metricsConfiguration,
            ("greengrassGroupName" Core..=)
              Prelude.<$> greengrassGroupName,
            ("flowActionsRoleArn" Core..=)
              Prelude.<$> flowActionsRoleArn,
            ("s3BucketName" Core..=) Prelude.<$> s3BucketName,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("definition" Core..= definition),
            Prelude.Just ("target" Core..= target)
          ]
      )

instance Core.ToPath CreateSystemInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSystemInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSystemInstanceResponse' smart constructor.
data CreateSystemInstanceResponse = CreateSystemInstanceResponse'
  { -- | The summary object that describes the new system instance.
    summary :: Prelude.Maybe SystemInstanceSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSystemInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'createSystemInstanceResponse_summary' - The summary object that describes the new system instance.
--
-- 'httpStatus', 'createSystemInstanceResponse_httpStatus' - The response's http status code.
newCreateSystemInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSystemInstanceResponse
newCreateSystemInstanceResponse pHttpStatus_ =
  CreateSystemInstanceResponse'
    { summary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The summary object that describes the new system instance.
createSystemInstanceResponse_summary :: Lens.Lens' CreateSystemInstanceResponse (Prelude.Maybe SystemInstanceSummary)
createSystemInstanceResponse_summary = Lens.lens (\CreateSystemInstanceResponse' {summary} -> summary) (\s@CreateSystemInstanceResponse' {} a -> s {summary = a} :: CreateSystemInstanceResponse)

-- | The response's http status code.
createSystemInstanceResponse_httpStatus :: Lens.Lens' CreateSystemInstanceResponse Prelude.Int
createSystemInstanceResponse_httpStatus = Lens.lens (\CreateSystemInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateSystemInstanceResponse' {} a -> s {httpStatus = a} :: CreateSystemInstanceResponse)

instance Prelude.NFData CreateSystemInstanceResponse
