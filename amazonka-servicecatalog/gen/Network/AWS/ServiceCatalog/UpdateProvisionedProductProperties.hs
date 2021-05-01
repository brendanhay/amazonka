{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests updates to the properties of the specified provisioned product.
module Network.AWS.ServiceCatalog.UpdateProvisionedProductProperties
  ( -- * Creating a Request
    UpdateProvisionedProductProperties (..),
    newUpdateProvisionedProductProperties,

    -- * Request Lenses
    updateProvisionedProductProperties_acceptLanguage,
    updateProvisionedProductProperties_provisionedProductId,
    updateProvisionedProductProperties_provisionedProductProperties,
    updateProvisionedProductProperties_idempotencyToken,

    -- * Destructuring the Response
    UpdateProvisionedProductPropertiesResponse (..),
    newUpdateProvisionedProductPropertiesResponse,

    -- * Response Lenses
    updateProvisionedProductPropertiesResponse_status,
    updateProvisionedProductPropertiesResponse_provisionedProductId,
    updateProvisionedProductPropertiesResponse_recordId,
    updateProvisionedProductPropertiesResponse_provisionedProductProperties,
    updateProvisionedProductPropertiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateProvisionedProductProperties' smart constructor.
data UpdateProvisionedProductProperties = UpdateProvisionedProductProperties'
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
    -- | A map that contains the provisioned product properties to be updated.
    --
    -- The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an
    -- administrator to call @UpdateProvisionedProductProperties@ to update the
    -- launch role that is associated with a provisioned product. This role is
    -- used when an end user calls a provisioning operation such as
    -- @UpdateProvisionedProduct@, @TerminateProvisionedProduct@, or
    -- @ExecuteProvisionedProductServiceAction@. Only a role ARN is valid. A
    -- user ARN is invalid.
    --
    -- The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user
    -- that has permission to see, update, terminate, and execute service
    -- actions in the provisioned product.
    --
    -- The administrator can change the owner of a provisioned product to
    -- another IAM user within the same account. Both end user owners and
    -- administrators can see ownership history of the provisioned product
    -- using the @ListRecordHistory@ API. The new owner can describe all past
    -- records for the provisioned product using the @DescribeRecord@ API. The
    -- previous owner can no longer use @DescribeRecord@, but can still see the
    -- product\'s history from when he was an owner using @ListRecordHistory@.
    --
    -- If a provisioned product ownership is assigned to an end user, they can
    -- see and perform any action through the API or Service Catalog console
    -- such as update, terminate, and execute service actions. If an end user
    -- provisions a product and the owner is updated to someone else, they will
    -- no longer be able to see or perform any actions through API or the
    -- Service Catalog console on that provisioned product.
    provisionedProductProperties :: Prelude.HashMap PropertyKey Prelude.Text,
    -- | The idempotency token that uniquely identifies the provisioning product
    -- update request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisionedProductProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'updateProvisionedProductProperties_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'provisionedProductId', 'updateProvisionedProductProperties_provisionedProductId' - The identifier of the provisioned product.
--
-- 'provisionedProductProperties', 'updateProvisionedProductProperties_provisionedProductProperties' - A map that contains the provisioned product properties to be updated.
--
-- The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an
-- administrator to call @UpdateProvisionedProductProperties@ to update the
-- launch role that is associated with a provisioned product. This role is
-- used when an end user calls a provisioning operation such as
-- @UpdateProvisionedProduct@, @TerminateProvisionedProduct@, or
-- @ExecuteProvisionedProductServiceAction@. Only a role ARN is valid. A
-- user ARN is invalid.
--
-- The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user
-- that has permission to see, update, terminate, and execute service
-- actions in the provisioned product.
--
-- The administrator can change the owner of a provisioned product to
-- another IAM user within the same account. Both end user owners and
-- administrators can see ownership history of the provisioned product
-- using the @ListRecordHistory@ API. The new owner can describe all past
-- records for the provisioned product using the @DescribeRecord@ API. The
-- previous owner can no longer use @DescribeRecord@, but can still see the
-- product\'s history from when he was an owner using @ListRecordHistory@.
--
-- If a provisioned product ownership is assigned to an end user, they can
-- see and perform any action through the API or Service Catalog console
-- such as update, terminate, and execute service actions. If an end user
-- provisions a product and the owner is updated to someone else, they will
-- no longer be able to see or perform any actions through API or the
-- Service Catalog console on that provisioned product.
--
-- 'idempotencyToken', 'updateProvisionedProductProperties_idempotencyToken' - The idempotency token that uniquely identifies the provisioning product
-- update request.
newUpdateProvisionedProductProperties ::
  -- | 'provisionedProductId'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  UpdateProvisionedProductProperties
newUpdateProvisionedProductProperties
  pProvisionedProductId_
  pIdempotencyToken_ =
    UpdateProvisionedProductProperties'
      { acceptLanguage =
          Prelude.Nothing,
        provisionedProductId =
          pProvisionedProductId_,
        provisionedProductProperties =
          Prelude.mempty,
        idempotencyToken = pIdempotencyToken_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateProvisionedProductProperties_acceptLanguage :: Lens.Lens' UpdateProvisionedProductProperties (Prelude.Maybe Prelude.Text)
updateProvisionedProductProperties_acceptLanguage = Lens.lens (\UpdateProvisionedProductProperties' {acceptLanguage} -> acceptLanguage) (\s@UpdateProvisionedProductProperties' {} a -> s {acceptLanguage = a} :: UpdateProvisionedProductProperties)

-- | The identifier of the provisioned product.
updateProvisionedProductProperties_provisionedProductId :: Lens.Lens' UpdateProvisionedProductProperties Prelude.Text
updateProvisionedProductProperties_provisionedProductId = Lens.lens (\UpdateProvisionedProductProperties' {provisionedProductId} -> provisionedProductId) (\s@UpdateProvisionedProductProperties' {} a -> s {provisionedProductId = a} :: UpdateProvisionedProductProperties)

-- | A map that contains the provisioned product properties to be updated.
--
-- The @LAUNCH_ROLE@ key accepts role ARNs. This key allows an
-- administrator to call @UpdateProvisionedProductProperties@ to update the
-- launch role that is associated with a provisioned product. This role is
-- used when an end user calls a provisioning operation such as
-- @UpdateProvisionedProduct@, @TerminateProvisionedProduct@, or
-- @ExecuteProvisionedProductServiceAction@. Only a role ARN is valid. A
-- user ARN is invalid.
--
-- The @OWNER@ key accepts user ARNs and role ARNs. The owner is the user
-- that has permission to see, update, terminate, and execute service
-- actions in the provisioned product.
--
-- The administrator can change the owner of a provisioned product to
-- another IAM user within the same account. Both end user owners and
-- administrators can see ownership history of the provisioned product
-- using the @ListRecordHistory@ API. The new owner can describe all past
-- records for the provisioned product using the @DescribeRecord@ API. The
-- previous owner can no longer use @DescribeRecord@, but can still see the
-- product\'s history from when he was an owner using @ListRecordHistory@.
--
-- If a provisioned product ownership is assigned to an end user, they can
-- see and perform any action through the API or Service Catalog console
-- such as update, terminate, and execute service actions. If an end user
-- provisions a product and the owner is updated to someone else, they will
-- no longer be able to see or perform any actions through API or the
-- Service Catalog console on that provisioned product.
updateProvisionedProductProperties_provisionedProductProperties :: Lens.Lens' UpdateProvisionedProductProperties (Prelude.HashMap PropertyKey Prelude.Text)
updateProvisionedProductProperties_provisionedProductProperties = Lens.lens (\UpdateProvisionedProductProperties' {provisionedProductProperties} -> provisionedProductProperties) (\s@UpdateProvisionedProductProperties' {} a -> s {provisionedProductProperties = a} :: UpdateProvisionedProductProperties) Prelude.. Prelude._Coerce

-- | The idempotency token that uniquely identifies the provisioning product
-- update request.
updateProvisionedProductProperties_idempotencyToken :: Lens.Lens' UpdateProvisionedProductProperties Prelude.Text
updateProvisionedProductProperties_idempotencyToken = Lens.lens (\UpdateProvisionedProductProperties' {idempotencyToken} -> idempotencyToken) (\s@UpdateProvisionedProductProperties' {} a -> s {idempotencyToken = a} :: UpdateProvisionedProductProperties)

instance
  Prelude.AWSRequest
    UpdateProvisionedProductProperties
  where
  type
    Rs UpdateProvisionedProductProperties =
      UpdateProvisionedProductPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProvisionedProductPropertiesResponse'
            Prelude.<$> (x Prelude..?> "Status")
              Prelude.<*> (x Prelude..?> "ProvisionedProductId")
              Prelude.<*> (x Prelude..?> "RecordId")
              Prelude.<*> ( x Prelude..?> "ProvisionedProductProperties"
                              Prelude..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateProvisionedProductProperties

instance
  Prelude.NFData
    UpdateProvisionedProductProperties

instance
  Prelude.ToHeaders
    UpdateProvisionedProductProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.UpdateProvisionedProductProperties" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    UpdateProvisionedProductProperties
  where
  toJSON UpdateProvisionedProductProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just
              ( "ProvisionedProductId"
                  Prelude..= provisionedProductId
              ),
            Prelude.Just
              ( "ProvisionedProductProperties"
                  Prelude..= provisionedProductProperties
              ),
            Prelude.Just
              ("IdempotencyToken" Prelude..= idempotencyToken)
          ]
      )

instance
  Prelude.ToPath
    UpdateProvisionedProductProperties
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateProvisionedProductProperties
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProvisionedProductPropertiesResponse' smart constructor.
data UpdateProvisionedProductPropertiesResponse = UpdateProvisionedProductPropertiesResponse'
  { -- | The status of the request.
    status :: Prelude.Maybe RecordStatus,
    -- | The provisioned product identifier.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the record.
    recordId :: Prelude.Maybe Prelude.Text,
    -- | A map that contains the properties updated.
    provisionedProductProperties :: Prelude.Maybe (Prelude.HashMap PropertyKey Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisionedProductPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateProvisionedProductPropertiesResponse_status' - The status of the request.
--
-- 'provisionedProductId', 'updateProvisionedProductPropertiesResponse_provisionedProductId' - The provisioned product identifier.
--
-- 'recordId', 'updateProvisionedProductPropertiesResponse_recordId' - The identifier of the record.
--
-- 'provisionedProductProperties', 'updateProvisionedProductPropertiesResponse_provisionedProductProperties' - A map that contains the properties updated.
--
-- 'httpStatus', 'updateProvisionedProductPropertiesResponse_httpStatus' - The response's http status code.
newUpdateProvisionedProductPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProvisionedProductPropertiesResponse
newUpdateProvisionedProductPropertiesResponse
  pHttpStatus_ =
    UpdateProvisionedProductPropertiesResponse'
      { status =
          Prelude.Nothing,
        provisionedProductId =
          Prelude.Nothing,
        recordId = Prelude.Nothing,
        provisionedProductProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the request.
updateProvisionedProductPropertiesResponse_status :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Prelude.Maybe RecordStatus)
updateProvisionedProductPropertiesResponse_status = Lens.lens (\UpdateProvisionedProductPropertiesResponse' {status} -> status) (\s@UpdateProvisionedProductPropertiesResponse' {} a -> s {status = a} :: UpdateProvisionedProductPropertiesResponse)

-- | The provisioned product identifier.
updateProvisionedProductPropertiesResponse_provisionedProductId :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Prelude.Maybe Prelude.Text)
updateProvisionedProductPropertiesResponse_provisionedProductId = Lens.lens (\UpdateProvisionedProductPropertiesResponse' {provisionedProductId} -> provisionedProductId) (\s@UpdateProvisionedProductPropertiesResponse' {} a -> s {provisionedProductId = a} :: UpdateProvisionedProductPropertiesResponse)

-- | The identifier of the record.
updateProvisionedProductPropertiesResponse_recordId :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Prelude.Maybe Prelude.Text)
updateProvisionedProductPropertiesResponse_recordId = Lens.lens (\UpdateProvisionedProductPropertiesResponse' {recordId} -> recordId) (\s@UpdateProvisionedProductPropertiesResponse' {} a -> s {recordId = a} :: UpdateProvisionedProductPropertiesResponse)

-- | A map that contains the properties updated.
updateProvisionedProductPropertiesResponse_provisionedProductProperties :: Lens.Lens' UpdateProvisionedProductPropertiesResponse (Prelude.Maybe (Prelude.HashMap PropertyKey Prelude.Text))
updateProvisionedProductPropertiesResponse_provisionedProductProperties = Lens.lens (\UpdateProvisionedProductPropertiesResponse' {provisionedProductProperties} -> provisionedProductProperties) (\s@UpdateProvisionedProductPropertiesResponse' {} a -> s {provisionedProductProperties = a} :: UpdateProvisionedProductPropertiesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
updateProvisionedProductPropertiesResponse_httpStatus :: Lens.Lens' UpdateProvisionedProductPropertiesResponse Prelude.Int
updateProvisionedProductPropertiesResponse_httpStatus = Lens.lens (\UpdateProvisionedProductPropertiesResponse' {httpStatus} -> httpStatus) (\s@UpdateProvisionedProductPropertiesResponse' {} a -> s {httpStatus = a} :: UpdateProvisionedProductPropertiesResponse)

instance
  Prelude.NFData
    UpdateProvisionedProductPropertiesResponse
