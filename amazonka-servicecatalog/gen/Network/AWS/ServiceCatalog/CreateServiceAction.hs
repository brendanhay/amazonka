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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreateServiceAction' smart constructor.
data CreateServiceAction = CreateServiceAction'
  { -- | The self-service action description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The self-service action name.
    name :: Prelude.Text,
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
    definition :: Prelude.HashMap ServiceActionDefinitionKey Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'definitionType'
  ServiceActionDefinitionType ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateServiceAction
newCreateServiceAction
  pName_
  pDefinitionType_
  pIdempotencyToken_ =
    CreateServiceAction'
      { description = Prelude.Nothing,
        acceptLanguage = Prelude.Nothing,
        name = pName_,
        definitionType = pDefinitionType_,
        definition = Prelude.mempty,
        idempotencyToken = pIdempotencyToken_
      }

-- | The self-service action description.
createServiceAction_description :: Lens.Lens' CreateServiceAction (Prelude.Maybe Prelude.Text)
createServiceAction_description = Lens.lens (\CreateServiceAction' {description} -> description) (\s@CreateServiceAction' {} a -> s {description = a} :: CreateServiceAction)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createServiceAction_acceptLanguage :: Lens.Lens' CreateServiceAction (Prelude.Maybe Prelude.Text)
createServiceAction_acceptLanguage = Lens.lens (\CreateServiceAction' {acceptLanguage} -> acceptLanguage) (\s@CreateServiceAction' {} a -> s {acceptLanguage = a} :: CreateServiceAction)

-- | The self-service action name.
createServiceAction_name :: Lens.Lens' CreateServiceAction Prelude.Text
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
createServiceAction_definition :: Lens.Lens' CreateServiceAction (Prelude.HashMap ServiceActionDefinitionKey Prelude.Text)
createServiceAction_definition = Lens.lens (\CreateServiceAction' {definition} -> definition) (\s@CreateServiceAction' {} a -> s {definition = a} :: CreateServiceAction) Prelude.. Lens._Coerce

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createServiceAction_idempotencyToken :: Lens.Lens' CreateServiceAction Prelude.Text
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
            Prelude.<$> (x Core..?> "ServiceActionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateServiceAction

instance Prelude.NFData CreateServiceAction

instance Core.ToHeaders CreateServiceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreateServiceAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateServiceAction where
  toJSON CreateServiceAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("DefinitionType" Core..= definitionType),
            Prelude.Just ("Definition" Core..= definition),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateServiceAction where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateServiceAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceActionResponse' smart constructor.
data CreateServiceActionResponse = CreateServiceActionResponse'
  { -- | An object containing information about the self-service action.
    serviceActionDetail :: Prelude.Maybe ServiceActionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateServiceActionResponse
newCreateServiceActionResponse pHttpStatus_ =
  CreateServiceActionResponse'
    { serviceActionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing information about the self-service action.
createServiceActionResponse_serviceActionDetail :: Lens.Lens' CreateServiceActionResponse (Prelude.Maybe ServiceActionDetail)
createServiceActionResponse_serviceActionDetail = Lens.lens (\CreateServiceActionResponse' {serviceActionDetail} -> serviceActionDetail) (\s@CreateServiceActionResponse' {} a -> s {serviceActionDetail = a} :: CreateServiceActionResponse)

-- | The response's http status code.
createServiceActionResponse_httpStatus :: Lens.Lens' CreateServiceActionResponse Prelude.Int
createServiceActionResponse_httpStatus = Lens.lens (\CreateServiceActionResponse' {httpStatus} -> httpStatus) (\s@CreateServiceActionResponse' {} a -> s {httpStatus = a} :: CreateServiceActionResponse)

instance Prelude.NFData CreateServiceActionResponse
