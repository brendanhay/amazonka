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
-- Module      : Amazonka.ServiceCatalog.CreateServiceAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a self-service action.
module Amazonka.ServiceCatalog.CreateServiceAction
  ( -- * Creating a Request
    CreateServiceAction (..),
    newCreateServiceAction,

    -- * Request Lenses
    createServiceAction_acceptLanguage,
    createServiceAction_description,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newCreateServiceAction' smart constructor.
data CreateServiceAction = CreateServiceAction'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The self-service action description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The self-service action name.
    name :: Prelude.Text,
    -- | The service action definition type. For example, @SSM_AUTOMATION@.
    definitionType :: ServiceActionDefinitionType,
    -- | The self-service action definition. Can be one of the following:
    --
    -- [Name]
    --     The name of the Amazon Web Services Systems Manager document (SSM
    --     document). For example, @AWS-RestartEC2Instance@.
    --
    --     If you are using a shared SSM document, you must provide the ARN
    --     instead of the name.
    --
    -- [Version]
    --     The Amazon Web Services Systems Manager automation document version.
    --     For example, @\"Version\": \"1\"@
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
-- 'acceptLanguage', 'createServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'description', 'createServiceAction_description' - The self-service action description.
--
-- 'name', 'createServiceAction_name' - The self-service action name.
--
-- 'definitionType', 'createServiceAction_definitionType' - The service action definition type. For example, @SSM_AUTOMATION@.
--
-- 'definition', 'createServiceAction_definition' - The self-service action definition. Can be one of the following:
--
-- [Name]
--     The name of the Amazon Web Services Systems Manager document (SSM
--     document). For example, @AWS-RestartEC2Instance@.
--
--     If you are using a shared SSM document, you must provide the ARN
--     instead of the name.
--
-- [Version]
--     The Amazon Web Services Systems Manager automation document version.
--     For example, @\"Version\": \"1\"@
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
      { acceptLanguage =
          Prelude.Nothing,
        description = Prelude.Nothing,
        name = pName_,
        definitionType = pDefinitionType_,
        definition = Prelude.mempty,
        idempotencyToken = pIdempotencyToken_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createServiceAction_acceptLanguage :: Lens.Lens' CreateServiceAction (Prelude.Maybe Prelude.Text)
createServiceAction_acceptLanguage = Lens.lens (\CreateServiceAction' {acceptLanguage} -> acceptLanguage) (\s@CreateServiceAction' {} a -> s {acceptLanguage = a} :: CreateServiceAction)

-- | The self-service action description.
createServiceAction_description :: Lens.Lens' CreateServiceAction (Prelude.Maybe Prelude.Text)
createServiceAction_description = Lens.lens (\CreateServiceAction' {description} -> description) (\s@CreateServiceAction' {} a -> s {description = a} :: CreateServiceAction)

-- | The self-service action name.
createServiceAction_name :: Lens.Lens' CreateServiceAction Prelude.Text
createServiceAction_name = Lens.lens (\CreateServiceAction' {name} -> name) (\s@CreateServiceAction' {} a -> s {name = a} :: CreateServiceAction)

-- | The service action definition type. For example, @SSM_AUTOMATION@.
createServiceAction_definitionType :: Lens.Lens' CreateServiceAction ServiceActionDefinitionType
createServiceAction_definitionType = Lens.lens (\CreateServiceAction' {definitionType} -> definitionType) (\s@CreateServiceAction' {} a -> s {definitionType = a} :: CreateServiceAction)

-- | The self-service action definition. Can be one of the following:
--
-- [Name]
--     The name of the Amazon Web Services Systems Manager document (SSM
--     document). For example, @AWS-RestartEC2Instance@.
--
--     If you are using a shared SSM document, you must provide the ARN
--     instead of the name.
--
-- [Version]
--     The Amazon Web Services Systems Manager automation document version.
--     For example, @\"Version\": \"1\"@
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
createServiceAction_definition = Lens.lens (\CreateServiceAction' {definition} -> definition) (\s@CreateServiceAction' {} a -> s {definition = a} :: CreateServiceAction) Prelude.. Lens.coerced

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createServiceAction_idempotencyToken :: Lens.Lens' CreateServiceAction Prelude.Text
createServiceAction_idempotencyToken = Lens.lens (\CreateServiceAction' {idempotencyToken} -> idempotencyToken) (\s@CreateServiceAction' {} a -> s {idempotencyToken = a} :: CreateServiceAction)

instance Core.AWSRequest CreateServiceAction where
  type
    AWSResponse CreateServiceAction =
      CreateServiceActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceActionResponse'
            Prelude.<$> (x Data..?> "ServiceActionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateServiceAction where
  hashWithSalt _salt CreateServiceAction' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` definitionType
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData CreateServiceAction where
  rnf CreateServiceAction' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf definitionType
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Data.ToHeaders CreateServiceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.CreateServiceAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateServiceAction where
  toJSON CreateServiceAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("DefinitionType" Data..= definitionType),
            Prelude.Just ("Definition" Data..= definition),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath CreateServiceAction where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServiceAction where
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

instance Prelude.NFData CreateServiceActionResponse where
  rnf CreateServiceActionResponse' {..} =
    Prelude.rnf serviceActionDetail
      `Prelude.seq` Prelude.rnf httpStatus
