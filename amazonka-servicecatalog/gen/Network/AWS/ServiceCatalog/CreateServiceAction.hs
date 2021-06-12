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
-- Module      : Network.AWS.ServiceCatalog.CreateServiceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a self-service action.
module Network.AWS.ServiceCatalog.CreateServiceAction
  ( -- * Creating a Request
    CreateServiceAction (..),
    newCreateServiceAction,

    -- * Request Lenses
    createServiceAction_description,
    createServiceAction_acceptLanguage,
    createServiceAction_name,
    createServiceAction_definitionType,
    createServiceAction_definition,
    createServiceAction_idempotencyToken,

    -- * Destructuring the Response
    CreateServiceActionResponse (..),
    newCreateServiceActionResponse,

    -- * Response Lenses
    createServiceActionResponse_serviceActionDetail,
    createServiceActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreateServiceAction' smart constructor.
data CreateServiceAction = CreateServiceAction'
  { -- | The self-service action description.
    description :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The self-service action name.
    name :: Core.Text,
    -- | The service action definition type. For example, @SSM_AUTOMATION@.
    definitionType :: ServiceActionDefinitionType,
    -- | The self-service action definition. Can be one of the following:
    --
    -- [Name]
    --     The name of the AWS Systems Manager document (SSM document). For
    --     example, @AWS-RestartEC2Instance@.
    --
    --     If you are using a shared SSM document, you must provide the ARN
    --     instead of the name.
    --
    -- [Version]
    --     The AWS Systems Manager automation document version. For example,
    --     @\"Version\": \"1\"@
    --
    -- [AssumeRole]
    --     The Amazon Resource Name (ARN) of the role that performs the
    --     self-service actions on your behalf. For example,
    --     @\"AssumeRole\": \"arn:aws:iam::12345678910:role\/ActionRole\"@.
    --
    --     To reuse the provisioned product launch role, set to
    --     @\"AssumeRole\": \"LAUNCH_ROLE\"@.
    --
    -- [Parameters]
    --     The list of parameters in JSON format.
    --
    --     For example:
    --     @[{\\\"Name\\\":\\\"InstanceId\\\",\\\"Type\\\":\\\"TARGET\\\"}]@ or
    --     @[{\\\"Name\\\":\\\"InstanceId\\\",\\\"Type\\\":\\\"TEXT_VALUE\\\"}]@.
    definition :: Core.HashMap ServiceActionDefinitionKey Core.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createServiceAction_description' - The self-service action description.
--
-- 'acceptLanguage', 'createServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'name', 'createServiceAction_name' - The self-service action name.
--
-- 'definitionType', 'createServiceAction_definitionType' - The service action definition type. For example, @SSM_AUTOMATION@.
--
-- 'definition', 'createServiceAction_definition' - The self-service action definition. Can be one of the following:
--
-- [Name]
--     The name of the AWS Systems Manager document (SSM document). For
--     example, @AWS-RestartEC2Instance@.
--
--     If you are using a shared SSM document, you must provide the ARN
--     instead of the name.
--
-- [Version]
--     The AWS Systems Manager automation document version. For example,
--     @\"Version\": \"1\"@
--
-- [AssumeRole]
--     The Amazon Resource Name (ARN) of the role that performs the
--     self-service actions on your behalf. For example,
--     @\"AssumeRole\": \"arn:aws:iam::12345678910:role\/ActionRole\"@.
--
--     To reuse the provisioned product launch role, set to
--     @\"AssumeRole\": \"LAUNCH_ROLE\"@.
--
-- [Parameters]
--     The list of parameters in JSON format.
--
--     For example:
--     @[{\\\"Name\\\":\\\"InstanceId\\\",\\\"Type\\\":\\\"TARGET\\\"}]@ or
--     @[{\\\"Name\\\":\\\"InstanceId\\\",\\\"Type\\\":\\\"TEXT_VALUE\\\"}]@.
--
-- 'idempotencyToken', 'createServiceAction_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newCreateServiceAction ::
  -- | 'name'
  Core.Text ->
  -- | 'definitionType'
  ServiceActionDefinitionType ->
  -- | 'idempotencyToken'
  Core.Text ->
  CreateServiceAction
newCreateServiceAction
  pName_
  pDefinitionType_
  pIdempotencyToken_ =
    CreateServiceAction'
      { description = Core.Nothing,
        acceptLanguage = Core.Nothing,
        name = pName_,
        definitionType = pDefinitionType_,
        definition = Core.mempty,
        idempotencyToken = pIdempotencyToken_
      }

-- | The self-service action description.
createServiceAction_description :: Lens.Lens' CreateServiceAction (Core.Maybe Core.Text)
createServiceAction_description = Lens.lens (\CreateServiceAction' {description} -> description) (\s@CreateServiceAction' {} a -> s {description = a} :: CreateServiceAction)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createServiceAction_acceptLanguage :: Lens.Lens' CreateServiceAction (Core.Maybe Core.Text)
createServiceAction_acceptLanguage = Lens.lens (\CreateServiceAction' {acceptLanguage} -> acceptLanguage) (\s@CreateServiceAction' {} a -> s {acceptLanguage = a} :: CreateServiceAction)

-- | The self-service action name.
createServiceAction_name :: Lens.Lens' CreateServiceAction Core.Text
createServiceAction_name = Lens.lens (\CreateServiceAction' {name} -> name) (\s@CreateServiceAction' {} a -> s {name = a} :: CreateServiceAction)

-- | The service action definition type. For example, @SSM_AUTOMATION@.
createServiceAction_definitionType :: Lens.Lens' CreateServiceAction ServiceActionDefinitionType
createServiceAction_definitionType = Lens.lens (\CreateServiceAction' {definitionType} -> definitionType) (\s@CreateServiceAction' {} a -> s {definitionType = a} :: CreateServiceAction)

-- | The self-service action definition. Can be one of the following:
--
-- [Name]
--     The name of the AWS Systems Manager document (SSM document). For
--     example, @AWS-RestartEC2Instance@.
--
--     If you are using a shared SSM document, you must provide the ARN
--     instead of the name.
--
-- [Version]
--     The AWS Systems Manager automation document version. For example,
--     @\"Version\": \"1\"@
--
-- [AssumeRole]
--     The Amazon Resource Name (ARN) of the role that performs the
--     self-service actions on your behalf. For example,
--     @\"AssumeRole\": \"arn:aws:iam::12345678910:role\/ActionRole\"@.
--
--     To reuse the provisioned product launch role, set to
--     @\"AssumeRole\": \"LAUNCH_ROLE\"@.
--
-- [Parameters]
--     The list of parameters in JSON format.
--
--     For example:
--     @[{\\\"Name\\\":\\\"InstanceId\\\",\\\"Type\\\":\\\"TARGET\\\"}]@ or
--     @[{\\\"Name\\\":\\\"InstanceId\\\",\\\"Type\\\":\\\"TEXT_VALUE\\\"}]@.
createServiceAction_definition :: Lens.Lens' CreateServiceAction (Core.HashMap ServiceActionDefinitionKey Core.Text)
createServiceAction_definition = Lens.lens (\CreateServiceAction' {definition} -> definition) (\s@CreateServiceAction' {} a -> s {definition = a} :: CreateServiceAction) Core.. Lens._Coerce

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createServiceAction_idempotencyToken :: Lens.Lens' CreateServiceAction Core.Text
createServiceAction_idempotencyToken = Lens.lens (\CreateServiceAction' {idempotencyToken} -> idempotencyToken) (\s@CreateServiceAction' {} a -> s {idempotencyToken = a} :: CreateServiceAction)

instance Core.AWSRequest CreateServiceAction where
  type
    AWSResponse CreateServiceAction =
      CreateServiceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceActionResponse'
            Core.<$> (x Core..?> "ServiceActionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateServiceAction

instance Core.NFData CreateServiceAction

instance Core.ToHeaders CreateServiceAction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreateServiceAction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateServiceAction where
  toJSON CreateServiceAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Name" Core..= name),
            Core.Just ("DefinitionType" Core..= definitionType),
            Core.Just ("Definition" Core..= definition),
            Core.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateServiceAction where
  toPath = Core.const "/"

instance Core.ToQuery CreateServiceAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateServiceActionResponse' smart constructor.
data CreateServiceActionResponse = CreateServiceActionResponse'
  { -- | An object containing information about the self-service action.
    serviceActionDetail :: Core.Maybe ServiceActionDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateServiceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceActionDetail', 'createServiceActionResponse_serviceActionDetail' - An object containing information about the self-service action.
--
-- 'httpStatus', 'createServiceActionResponse_httpStatus' - The response's http status code.
newCreateServiceActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateServiceActionResponse
newCreateServiceActionResponse pHttpStatus_ =
  CreateServiceActionResponse'
    { serviceActionDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing information about the self-service action.
createServiceActionResponse_serviceActionDetail :: Lens.Lens' CreateServiceActionResponse (Core.Maybe ServiceActionDetail)
createServiceActionResponse_serviceActionDetail = Lens.lens (\CreateServiceActionResponse' {serviceActionDetail} -> serviceActionDetail) (\s@CreateServiceActionResponse' {} a -> s {serviceActionDetail = a} :: CreateServiceActionResponse)

-- | The response's http status code.
createServiceActionResponse_httpStatus :: Lens.Lens' CreateServiceActionResponse Core.Int
createServiceActionResponse_httpStatus = Lens.lens (\CreateServiceActionResponse' {httpStatus} -> httpStatus) (\s@CreateServiceActionResponse' {} a -> s {httpStatus = a} :: CreateServiceActionResponse)

instance Core.NFData CreateServiceActionResponse
