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
-- Module      : Amazonka.SageMaker.CreateFlowDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a flow definition.
module Amazonka.SageMaker.CreateFlowDefinition
  ( -- * Creating a Request
    CreateFlowDefinition (..),
    newCreateFlowDefinition,

    -- * Request Lenses
    createFlowDefinition_tags,
    createFlowDefinition_humanLoopActivationConfig,
    createFlowDefinition_humanLoopRequestSource,
    createFlowDefinition_flowDefinitionName,
    createFlowDefinition_humanLoopConfig,
    createFlowDefinition_outputConfig,
    createFlowDefinition_roleArn,

    -- * Destructuring the Response
    CreateFlowDefinitionResponse (..),
    newCreateFlowDefinitionResponse,

    -- * Response Lenses
    createFlowDefinitionResponse_httpStatus,
    createFlowDefinitionResponse_flowDefinitionArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateFlowDefinition' smart constructor.
data CreateFlowDefinition = CreateFlowDefinition'
  { -- | An array of key-value pairs that contain metadata to help you categorize
    -- and organize a flow definition. Each tag consists of a key and a value,
    -- both of which you define.
    tags :: Prelude.Maybe [Tag],
    -- | An object containing information about the events that trigger a human
    -- workflow.
    humanLoopActivationConfig :: Prelude.Maybe HumanLoopActivationConfig,
    -- | Container for configuring the source of human task requests. Use to
    -- specify if Amazon Rekognition or Amazon Textract is used as an
    -- integration source.
    humanLoopRequestSource :: Prelude.Maybe HumanLoopRequestSource,
    -- | The name of your flow definition.
    flowDefinitionName :: Prelude.Text,
    -- | An object containing information about the tasks the human reviewers
    -- will perform.
    humanLoopConfig :: HumanLoopConfig,
    -- | An object containing information about where the human review results
    -- will be uploaded.
    outputConfig :: FlowDefinitionOutputConfig,
    -- | The Amazon Resource Name (ARN) of the role needed to call other services
    -- on your behalf. For example,
    -- @arn:aws:iam::1234567890:role\/service-role\/AmazonSageMaker-ExecutionRole-20180111T151298@.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFlowDefinition_tags' - An array of key-value pairs that contain metadata to help you categorize
-- and organize a flow definition. Each tag consists of a key and a value,
-- both of which you define.
--
-- 'humanLoopActivationConfig', 'createFlowDefinition_humanLoopActivationConfig' - An object containing information about the events that trigger a human
-- workflow.
--
-- 'humanLoopRequestSource', 'createFlowDefinition_humanLoopRequestSource' - Container for configuring the source of human task requests. Use to
-- specify if Amazon Rekognition or Amazon Textract is used as an
-- integration source.
--
-- 'flowDefinitionName', 'createFlowDefinition_flowDefinitionName' - The name of your flow definition.
--
-- 'humanLoopConfig', 'createFlowDefinition_humanLoopConfig' - An object containing information about the tasks the human reviewers
-- will perform.
--
-- 'outputConfig', 'createFlowDefinition_outputConfig' - An object containing information about where the human review results
-- will be uploaded.
--
-- 'roleArn', 'createFlowDefinition_roleArn' - The Amazon Resource Name (ARN) of the role needed to call other services
-- on your behalf. For example,
-- @arn:aws:iam::1234567890:role\/service-role\/AmazonSageMaker-ExecutionRole-20180111T151298@.
newCreateFlowDefinition ::
  -- | 'flowDefinitionName'
  Prelude.Text ->
  -- | 'humanLoopConfig'
  HumanLoopConfig ->
  -- | 'outputConfig'
  FlowDefinitionOutputConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateFlowDefinition
newCreateFlowDefinition
  pFlowDefinitionName_
  pHumanLoopConfig_
  pOutputConfig_
  pRoleArn_ =
    CreateFlowDefinition'
      { tags = Prelude.Nothing,
        humanLoopActivationConfig = Prelude.Nothing,
        humanLoopRequestSource = Prelude.Nothing,
        flowDefinitionName = pFlowDefinitionName_,
        humanLoopConfig = pHumanLoopConfig_,
        outputConfig = pOutputConfig_,
        roleArn = pRoleArn_
      }

-- | An array of key-value pairs that contain metadata to help you categorize
-- and organize a flow definition. Each tag consists of a key and a value,
-- both of which you define.
createFlowDefinition_tags :: Lens.Lens' CreateFlowDefinition (Prelude.Maybe [Tag])
createFlowDefinition_tags = Lens.lens (\CreateFlowDefinition' {tags} -> tags) (\s@CreateFlowDefinition' {} a -> s {tags = a} :: CreateFlowDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An object containing information about the events that trigger a human
-- workflow.
createFlowDefinition_humanLoopActivationConfig :: Lens.Lens' CreateFlowDefinition (Prelude.Maybe HumanLoopActivationConfig)
createFlowDefinition_humanLoopActivationConfig = Lens.lens (\CreateFlowDefinition' {humanLoopActivationConfig} -> humanLoopActivationConfig) (\s@CreateFlowDefinition' {} a -> s {humanLoopActivationConfig = a} :: CreateFlowDefinition)

-- | Container for configuring the source of human task requests. Use to
-- specify if Amazon Rekognition or Amazon Textract is used as an
-- integration source.
createFlowDefinition_humanLoopRequestSource :: Lens.Lens' CreateFlowDefinition (Prelude.Maybe HumanLoopRequestSource)
createFlowDefinition_humanLoopRequestSource = Lens.lens (\CreateFlowDefinition' {humanLoopRequestSource} -> humanLoopRequestSource) (\s@CreateFlowDefinition' {} a -> s {humanLoopRequestSource = a} :: CreateFlowDefinition)

-- | The name of your flow definition.
createFlowDefinition_flowDefinitionName :: Lens.Lens' CreateFlowDefinition Prelude.Text
createFlowDefinition_flowDefinitionName = Lens.lens (\CreateFlowDefinition' {flowDefinitionName} -> flowDefinitionName) (\s@CreateFlowDefinition' {} a -> s {flowDefinitionName = a} :: CreateFlowDefinition)

-- | An object containing information about the tasks the human reviewers
-- will perform.
createFlowDefinition_humanLoopConfig :: Lens.Lens' CreateFlowDefinition HumanLoopConfig
createFlowDefinition_humanLoopConfig = Lens.lens (\CreateFlowDefinition' {humanLoopConfig} -> humanLoopConfig) (\s@CreateFlowDefinition' {} a -> s {humanLoopConfig = a} :: CreateFlowDefinition)

-- | An object containing information about where the human review results
-- will be uploaded.
createFlowDefinition_outputConfig :: Lens.Lens' CreateFlowDefinition FlowDefinitionOutputConfig
createFlowDefinition_outputConfig = Lens.lens (\CreateFlowDefinition' {outputConfig} -> outputConfig) (\s@CreateFlowDefinition' {} a -> s {outputConfig = a} :: CreateFlowDefinition)

-- | The Amazon Resource Name (ARN) of the role needed to call other services
-- on your behalf. For example,
-- @arn:aws:iam::1234567890:role\/service-role\/AmazonSageMaker-ExecutionRole-20180111T151298@.
createFlowDefinition_roleArn :: Lens.Lens' CreateFlowDefinition Prelude.Text
createFlowDefinition_roleArn = Lens.lens (\CreateFlowDefinition' {roleArn} -> roleArn) (\s@CreateFlowDefinition' {} a -> s {roleArn = a} :: CreateFlowDefinition)

instance Core.AWSRequest CreateFlowDefinition where
  type
    AWSResponse CreateFlowDefinition =
      CreateFlowDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFlowDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "FlowDefinitionArn")
      )

instance Prelude.Hashable CreateFlowDefinition where
  hashWithSalt _salt CreateFlowDefinition' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` humanLoopActivationConfig
      `Prelude.hashWithSalt` humanLoopRequestSource
      `Prelude.hashWithSalt` flowDefinitionName
      `Prelude.hashWithSalt` humanLoopConfig
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateFlowDefinition where
  rnf CreateFlowDefinition' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf humanLoopActivationConfig
      `Prelude.seq` Prelude.rnf humanLoopRequestSource
      `Prelude.seq` Prelude.rnf flowDefinitionName
      `Prelude.seq` Prelude.rnf humanLoopConfig
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf roleArn

instance Core.ToHeaders CreateFlowDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateFlowDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFlowDefinition where
  toJSON CreateFlowDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("HumanLoopActivationConfig" Core..=)
              Prelude.<$> humanLoopActivationConfig,
            ("HumanLoopRequestSource" Core..=)
              Prelude.<$> humanLoopRequestSource,
            Prelude.Just
              ("FlowDefinitionName" Core..= flowDefinitionName),
            Prelude.Just
              ("HumanLoopConfig" Core..= humanLoopConfig),
            Prelude.Just ("OutputConfig" Core..= outputConfig),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateFlowDefinition where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFlowDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFlowDefinitionResponse' smart constructor.
data CreateFlowDefinitionResponse = CreateFlowDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the flow definition you create.
    flowDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFlowDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'flowDefinitionArn', 'createFlowDefinitionResponse_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow definition you create.
newCreateFlowDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'flowDefinitionArn'
  Prelude.Text ->
  CreateFlowDefinitionResponse
newCreateFlowDefinitionResponse
  pHttpStatus_
  pFlowDefinitionArn_ =
    CreateFlowDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        flowDefinitionArn = pFlowDefinitionArn_
      }

-- | The response's http status code.
createFlowDefinitionResponse_httpStatus :: Lens.Lens' CreateFlowDefinitionResponse Prelude.Int
createFlowDefinitionResponse_httpStatus = Lens.lens (\CreateFlowDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateFlowDefinitionResponse' {} a -> s {httpStatus = a} :: CreateFlowDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the flow definition you create.
createFlowDefinitionResponse_flowDefinitionArn :: Lens.Lens' CreateFlowDefinitionResponse Prelude.Text
createFlowDefinitionResponse_flowDefinitionArn = Lens.lens (\CreateFlowDefinitionResponse' {flowDefinitionArn} -> flowDefinitionArn) (\s@CreateFlowDefinitionResponse' {} a -> s {flowDefinitionArn = a} :: CreateFlowDefinitionResponse)

instance Prelude.NFData CreateFlowDefinitionResponse where
  rnf CreateFlowDefinitionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf flowDefinitionArn
