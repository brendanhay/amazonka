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
-- Module      : Amazonka.WorkMail.UpdateResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates data for the resource. To have the latest information, it must
-- be preceded by a DescribeResource call. The dataset in the request
-- should be the one expected when performing another @DescribeResource@
-- call.
module Amazonka.WorkMail.UpdateResource
  ( -- * Creating a Request
    UpdateResource (..),
    newUpdateResource,

    -- * Request Lenses
    updateResource_bookingOptions,
    updateResource_name,
    updateResource_organizationId,
    updateResource_resourceId,

    -- * Destructuring the Response
    UpdateResourceResponse (..),
    newUpdateResourceResponse,

    -- * Response Lenses
    updateResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { -- | The resource\'s booking options to be updated.
    bookingOptions :: Prelude.Maybe BookingOptions,
    -- | The name of the resource to be updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier associated with the organization for which the resource
    -- is updated.
    organizationId :: Prelude.Text,
    -- | The identifier of the resource to be updated.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bookingOptions', 'updateResource_bookingOptions' - The resource\'s booking options to be updated.
--
-- 'name', 'updateResource_name' - The name of the resource to be updated.
--
-- 'organizationId', 'updateResource_organizationId' - The identifier associated with the organization for which the resource
-- is updated.
--
-- 'resourceId', 'updateResource_resourceId' - The identifier of the resource to be updated.
newUpdateResource ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  UpdateResource
newUpdateResource pOrganizationId_ pResourceId_ =
  UpdateResource'
    { bookingOptions = Prelude.Nothing,
      name = Prelude.Nothing,
      organizationId = pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The resource\'s booking options to be updated.
updateResource_bookingOptions :: Lens.Lens' UpdateResource (Prelude.Maybe BookingOptions)
updateResource_bookingOptions = Lens.lens (\UpdateResource' {bookingOptions} -> bookingOptions) (\s@UpdateResource' {} a -> s {bookingOptions = a} :: UpdateResource)

-- | The name of the resource to be updated.
updateResource_name :: Lens.Lens' UpdateResource (Prelude.Maybe Prelude.Text)
updateResource_name = Lens.lens (\UpdateResource' {name} -> name) (\s@UpdateResource' {} a -> s {name = a} :: UpdateResource)

-- | The identifier associated with the organization for which the resource
-- is updated.
updateResource_organizationId :: Lens.Lens' UpdateResource Prelude.Text
updateResource_organizationId = Lens.lens (\UpdateResource' {organizationId} -> organizationId) (\s@UpdateResource' {} a -> s {organizationId = a} :: UpdateResource)

-- | The identifier of the resource to be updated.
updateResource_resourceId :: Lens.Lens' UpdateResource Prelude.Text
updateResource_resourceId = Lens.lens (\UpdateResource' {resourceId} -> resourceId) (\s@UpdateResource' {} a -> s {resourceId = a} :: UpdateResource)

instance Core.AWSRequest UpdateResource where
  type
    AWSResponse UpdateResource =
      UpdateResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResource where
  hashWithSalt _salt UpdateResource' {..} =
    _salt
      `Prelude.hashWithSalt` bookingOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData UpdateResource where
  rnf UpdateResource' {..} =
    Prelude.rnf bookingOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders UpdateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.UpdateResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResource where
  toJSON UpdateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BookingOptions" Data..=)
              Prelude.<$> bookingOptions,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath UpdateResource where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceResponse' smart constructor.
data UpdateResourceResponse = UpdateResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceResponse_httpStatus' - The response's http status code.
newUpdateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceResponse
newUpdateResourceResponse pHttpStatus_ =
  UpdateResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateResourceResponse_httpStatus :: Lens.Lens' UpdateResourceResponse Prelude.Int
updateResourceResponse_httpStatus = Lens.lens (\UpdateResourceResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceResponse' {} a -> s {httpStatus = a} :: UpdateResourceResponse)

instance Prelude.NFData UpdateResourceResponse where
  rnf UpdateResourceResponse' {..} =
    Prelude.rnf httpStatus
