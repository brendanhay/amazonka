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
-- Module      : Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds the default parameters for a specific self-service action on a
-- specific provisioned product and returns a map of the results to the
-- user.
module Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
  ( -- * Creating a Request
    DescribeServiceActionExecutionParameters (..),
    newDescribeServiceActionExecutionParameters,

    -- * Request Lenses
    describeServiceActionExecutionParameters_acceptLanguage,
    describeServiceActionExecutionParameters_provisionedProductId,
    describeServiceActionExecutionParameters_serviceActionId,

    -- * Destructuring the Response
    DescribeServiceActionExecutionParametersResponse (..),
    newDescribeServiceActionExecutionParametersResponse,

    -- * Response Lenses
    describeServiceActionExecutionParametersResponse_serviceActionParameters,
    describeServiceActionExecutionParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeServiceActionExecutionParameters' smart constructor.
data DescribeServiceActionExecutionParameters = DescribeServiceActionExecutionParameters'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioned product.
    provisionedProductId :: Prelude.Text,
    -- | The self-service action identifier.
    serviceActionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceActionExecutionParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeServiceActionExecutionParameters_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'provisionedProductId', 'describeServiceActionExecutionParameters_provisionedProductId' - The identifier of the provisioned product.
--
-- 'serviceActionId', 'describeServiceActionExecutionParameters_serviceActionId' - The self-service action identifier.
newDescribeServiceActionExecutionParameters ::
  -- | 'provisionedProductId'
  Prelude.Text ->
  -- | 'serviceActionId'
  Prelude.Text ->
  DescribeServiceActionExecutionParameters
newDescribeServiceActionExecutionParameters
  pProvisionedProductId_
  pServiceActionId_ =
    DescribeServiceActionExecutionParameters'
      { acceptLanguage =
          Prelude.Nothing,
        provisionedProductId =
          pProvisionedProductId_,
        serviceActionId =
          pServiceActionId_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeServiceActionExecutionParameters_acceptLanguage :: Lens.Lens' DescribeServiceActionExecutionParameters (Prelude.Maybe Prelude.Text)
describeServiceActionExecutionParameters_acceptLanguage = Lens.lens (\DescribeServiceActionExecutionParameters' {acceptLanguage} -> acceptLanguage) (\s@DescribeServiceActionExecutionParameters' {} a -> s {acceptLanguage = a} :: DescribeServiceActionExecutionParameters)

-- | The identifier of the provisioned product.
describeServiceActionExecutionParameters_provisionedProductId :: Lens.Lens' DescribeServiceActionExecutionParameters Prelude.Text
describeServiceActionExecutionParameters_provisionedProductId = Lens.lens (\DescribeServiceActionExecutionParameters' {provisionedProductId} -> provisionedProductId) (\s@DescribeServiceActionExecutionParameters' {} a -> s {provisionedProductId = a} :: DescribeServiceActionExecutionParameters)

-- | The self-service action identifier.
describeServiceActionExecutionParameters_serviceActionId :: Lens.Lens' DescribeServiceActionExecutionParameters Prelude.Text
describeServiceActionExecutionParameters_serviceActionId = Lens.lens (\DescribeServiceActionExecutionParameters' {serviceActionId} -> serviceActionId) (\s@DescribeServiceActionExecutionParameters' {} a -> s {serviceActionId = a} :: DescribeServiceActionExecutionParameters)

instance
  Core.AWSRequest
    DescribeServiceActionExecutionParameters
  where
  type
    AWSResponse
      DescribeServiceActionExecutionParameters =
      DescribeServiceActionExecutionParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceActionExecutionParametersResponse'
            Prelude.<$> ( x Core..?> "ServiceActionParameters"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeServiceActionExecutionParameters

instance
  Prelude.NFData
    DescribeServiceActionExecutionParameters

instance
  Core.ToHeaders
    DescribeServiceActionExecutionParameters
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeServiceActionExecutionParameters" ::
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
    DescribeServiceActionExecutionParameters
  where
  toJSON DescribeServiceActionExecutionParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just
              ( "ProvisionedProductId"
                  Core..= provisionedProductId
              ),
            Prelude.Just
              ("ServiceActionId" Core..= serviceActionId)
          ]
      )

instance
  Core.ToPath
    DescribeServiceActionExecutionParameters
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeServiceActionExecutionParameters
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServiceActionExecutionParametersResponse' smart constructor.
data DescribeServiceActionExecutionParametersResponse = DescribeServiceActionExecutionParametersResponse'
  { -- | The parameters of the self-service action.
    serviceActionParameters :: Prelude.Maybe [ExecutionParameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceActionExecutionParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceActionParameters', 'describeServiceActionExecutionParametersResponse_serviceActionParameters' - The parameters of the self-service action.
--
-- 'httpStatus', 'describeServiceActionExecutionParametersResponse_httpStatus' - The response's http status code.
newDescribeServiceActionExecutionParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceActionExecutionParametersResponse
newDescribeServiceActionExecutionParametersResponse
  pHttpStatus_ =
    DescribeServiceActionExecutionParametersResponse'
      { serviceActionParameters =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The parameters of the self-service action.
describeServiceActionExecutionParametersResponse_serviceActionParameters :: Lens.Lens' DescribeServiceActionExecutionParametersResponse (Prelude.Maybe [ExecutionParameter])
describeServiceActionExecutionParametersResponse_serviceActionParameters = Lens.lens (\DescribeServiceActionExecutionParametersResponse' {serviceActionParameters} -> serviceActionParameters) (\s@DescribeServiceActionExecutionParametersResponse' {} a -> s {serviceActionParameters = a} :: DescribeServiceActionExecutionParametersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeServiceActionExecutionParametersResponse_httpStatus :: Lens.Lens' DescribeServiceActionExecutionParametersResponse Prelude.Int
describeServiceActionExecutionParametersResponse_httpStatus = Lens.lens (\DescribeServiceActionExecutionParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceActionExecutionParametersResponse' {} a -> s {httpStatus = a} :: DescribeServiceActionExecutionParametersResponse)

instance
  Prelude.NFData
    DescribeServiceActionExecutionParametersResponse
