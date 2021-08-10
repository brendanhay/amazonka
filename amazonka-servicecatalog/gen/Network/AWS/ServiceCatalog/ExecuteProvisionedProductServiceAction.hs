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
-- Module      : Network.AWS.ServiceCatalog.ExecuteProvisionedProductServiceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes a self-service action against a provisioned product.
module Network.AWS.ServiceCatalog.ExecuteProvisionedProductServiceAction
  ( -- * Creating a Request
    ExecuteProvisionedProductServiceAction (..),
    newExecuteProvisionedProductServiceAction,

    -- * Request Lenses
    executeProvisionedProductServiceAction_parameters,
    executeProvisionedProductServiceAction_acceptLanguage,
    executeProvisionedProductServiceAction_provisionedProductId,
    executeProvisionedProductServiceAction_serviceActionId,
    executeProvisionedProductServiceAction_executeToken,

    -- * Destructuring the Response
    ExecuteProvisionedProductServiceActionResponse (..),
    newExecuteProvisionedProductServiceActionResponse,

    -- * Response Lenses
    executeProvisionedProductServiceActionResponse_recordDetail,
    executeProvisionedProductServiceActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newExecuteProvisionedProductServiceAction' smart constructor.
data ExecuteProvisionedProductServiceAction = ExecuteProvisionedProductServiceAction'
  { -- | A map of all self-service action parameters and their values. If a
    -- provided parameter is of a special type, such as @TARGET@, the provided
    -- value will override the default value generated by AWS Service Catalog.
    -- If the parameters field is not provided, no additional parameters are
    -- passed and default values will be used for any special parameters such
    -- as @TARGET@.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioned product.
    provisionedProductId :: Prelude.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Prelude.Text,
    -- | An idempotency token that uniquely identifies the execute request.
    executeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteProvisionedProductServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'executeProvisionedProductServiceAction_parameters' - A map of all self-service action parameters and their values. If a
-- provided parameter is of a special type, such as @TARGET@, the provided
-- value will override the default value generated by AWS Service Catalog.
-- If the parameters field is not provided, no additional parameters are
-- passed and default values will be used for any special parameters such
-- as @TARGET@.
--
-- 'acceptLanguage', 'executeProvisionedProductServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'provisionedProductId', 'executeProvisionedProductServiceAction_provisionedProductId' - The identifier of the provisioned product.
--
-- 'serviceActionId', 'executeProvisionedProductServiceAction_serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
--
-- 'executeToken', 'executeProvisionedProductServiceAction_executeToken' - An idempotency token that uniquely identifies the execute request.
newExecuteProvisionedProductServiceAction ::
  -- | 'provisionedProductId'
  Prelude.Text ->
  -- | 'serviceActionId'
  Prelude.Text ->
  -- | 'executeToken'
  Prelude.Text ->
  ExecuteProvisionedProductServiceAction
newExecuteProvisionedProductServiceAction
  pProvisionedProductId_
  pServiceActionId_
  pExecuteToken_ =
    ExecuteProvisionedProductServiceAction'
      { parameters =
          Prelude.Nothing,
        acceptLanguage = Prelude.Nothing,
        provisionedProductId =
          pProvisionedProductId_,
        serviceActionId = pServiceActionId_,
        executeToken = pExecuteToken_
      }

-- | A map of all self-service action parameters and their values. If a
-- provided parameter is of a special type, such as @TARGET@, the provided
-- value will override the default value generated by AWS Service Catalog.
-- If the parameters field is not provided, no additional parameters are
-- passed and default values will be used for any special parameters such
-- as @TARGET@.
executeProvisionedProductServiceAction_parameters :: Lens.Lens' ExecuteProvisionedProductServiceAction (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
executeProvisionedProductServiceAction_parameters = Lens.lens (\ExecuteProvisionedProductServiceAction' {parameters} -> parameters) (\s@ExecuteProvisionedProductServiceAction' {} a -> s {parameters = a} :: ExecuteProvisionedProductServiceAction) Prelude.. Lens.mapping Lens._Coerce

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
executeProvisionedProductServiceAction_acceptLanguage :: Lens.Lens' ExecuteProvisionedProductServiceAction (Prelude.Maybe Prelude.Text)
executeProvisionedProductServiceAction_acceptLanguage = Lens.lens (\ExecuteProvisionedProductServiceAction' {acceptLanguage} -> acceptLanguage) (\s@ExecuteProvisionedProductServiceAction' {} a -> s {acceptLanguage = a} :: ExecuteProvisionedProductServiceAction)

-- | The identifier of the provisioned product.
executeProvisionedProductServiceAction_provisionedProductId :: Lens.Lens' ExecuteProvisionedProductServiceAction Prelude.Text
executeProvisionedProductServiceAction_provisionedProductId = Lens.lens (\ExecuteProvisionedProductServiceAction' {provisionedProductId} -> provisionedProductId) (\s@ExecuteProvisionedProductServiceAction' {} a -> s {provisionedProductId = a} :: ExecuteProvisionedProductServiceAction)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
executeProvisionedProductServiceAction_serviceActionId :: Lens.Lens' ExecuteProvisionedProductServiceAction Prelude.Text
executeProvisionedProductServiceAction_serviceActionId = Lens.lens (\ExecuteProvisionedProductServiceAction' {serviceActionId} -> serviceActionId) (\s@ExecuteProvisionedProductServiceAction' {} a -> s {serviceActionId = a} :: ExecuteProvisionedProductServiceAction)

-- | An idempotency token that uniquely identifies the execute request.
executeProvisionedProductServiceAction_executeToken :: Lens.Lens' ExecuteProvisionedProductServiceAction Prelude.Text
executeProvisionedProductServiceAction_executeToken = Lens.lens (\ExecuteProvisionedProductServiceAction' {executeToken} -> executeToken) (\s@ExecuteProvisionedProductServiceAction' {} a -> s {executeToken = a} :: ExecuteProvisionedProductServiceAction)

instance
  Core.AWSRequest
    ExecuteProvisionedProductServiceAction
  where
  type
    AWSResponse
      ExecuteProvisionedProductServiceAction =
      ExecuteProvisionedProductServiceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteProvisionedProductServiceActionResponse'
            Prelude.<$> (x Core..?> "RecordDetail")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExecuteProvisionedProductServiceAction

instance
  Prelude.NFData
    ExecuteProvisionedProductServiceAction

instance
  Core.ToHeaders
    ExecuteProvisionedProductServiceAction
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ExecuteProvisionedProductServiceAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ExecuteProvisionedProductServiceAction
  where
  toJSON ExecuteProvisionedProductServiceAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Parameters" Core..=) Prelude.<$> parameters,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just
              ( "ProvisionedProductId"
                  Core..= provisionedProductId
              ),
            Prelude.Just
              ("ServiceActionId" Core..= serviceActionId),
            Prelude.Just ("ExecuteToken" Core..= executeToken)
          ]
      )

instance
  Core.ToPath
    ExecuteProvisionedProductServiceAction
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ExecuteProvisionedProductServiceAction
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteProvisionedProductServiceActionResponse' smart constructor.
data ExecuteProvisionedProductServiceActionResponse = ExecuteProvisionedProductServiceActionResponse'
  { -- | An object containing detailed information about the result of
    -- provisioning the product.
    recordDetail :: Prelude.Maybe RecordDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteProvisionedProductServiceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDetail', 'executeProvisionedProductServiceActionResponse_recordDetail' - An object containing detailed information about the result of
-- provisioning the product.
--
-- 'httpStatus', 'executeProvisionedProductServiceActionResponse_httpStatus' - The response's http status code.
newExecuteProvisionedProductServiceActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteProvisionedProductServiceActionResponse
newExecuteProvisionedProductServiceActionResponse
  pHttpStatus_ =
    ExecuteProvisionedProductServiceActionResponse'
      { recordDetail =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object containing detailed information about the result of
-- provisioning the product.
executeProvisionedProductServiceActionResponse_recordDetail :: Lens.Lens' ExecuteProvisionedProductServiceActionResponse (Prelude.Maybe RecordDetail)
executeProvisionedProductServiceActionResponse_recordDetail = Lens.lens (\ExecuteProvisionedProductServiceActionResponse' {recordDetail} -> recordDetail) (\s@ExecuteProvisionedProductServiceActionResponse' {} a -> s {recordDetail = a} :: ExecuteProvisionedProductServiceActionResponse)

-- | The response's http status code.
executeProvisionedProductServiceActionResponse_httpStatus :: Lens.Lens' ExecuteProvisionedProductServiceActionResponse Prelude.Int
executeProvisionedProductServiceActionResponse_httpStatus = Lens.lens (\ExecuteProvisionedProductServiceActionResponse' {httpStatus} -> httpStatus) (\s@ExecuteProvisionedProductServiceActionResponse' {} a -> s {httpStatus = a} :: ExecuteProvisionedProductServiceActionResponse)

instance
  Prelude.NFData
    ExecuteProvisionedProductServiceActionResponse
