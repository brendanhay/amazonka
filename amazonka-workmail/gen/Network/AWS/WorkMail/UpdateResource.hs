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
-- Module      : Network.AWS.WorkMail.UpdateResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates data for the resource. To have the latest information, it must
-- be preceded by a DescribeResource call. The dataset in the request
-- should be the one expected when performing another @DescribeResource@
-- call.
module Network.AWS.WorkMail.UpdateResource
  ( -- * Creating a Request
    UpdateResource (..),
    newUpdateResource,

    -- * Request Lenses
    updateResource_name,
    updateResource_bookingOptions,
    updateResource_organizationId,
    updateResource_resourceId,

    -- * Destructuring the Response
    UpdateResourceResponse (..),
    newUpdateResourceResponse,

    -- * Response Lenses
    updateResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { -- | The name of the resource to be updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | The resource\'s booking options to be updated.
    bookingOptions :: Prelude.Maybe BookingOptions,
    -- | The identifier associated with the organization for which the resource
    -- is updated.
    organizationId :: Prelude.Text,
    -- | The identifier of the resource to be updated.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateResource_name' - The name of the resource to be updated.
--
-- 'bookingOptions', 'updateResource_bookingOptions' - The resource\'s booking options to be updated.
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
    { name = Prelude.Nothing,
      bookingOptions = Prelude.Nothing,
      organizationId = pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The name of the resource to be updated.
updateResource_name :: Lens.Lens' UpdateResource (Prelude.Maybe Prelude.Text)
updateResource_name = Lens.lens (\UpdateResource' {name} -> name) (\s@UpdateResource' {} a -> s {name = a} :: UpdateResource)

-- | The resource\'s booking options to be updated.
updateResource_bookingOptions :: Lens.Lens' UpdateResource (Prelude.Maybe BookingOptions)
updateResource_bookingOptions = Lens.lens (\UpdateResource' {bookingOptions} -> bookingOptions) (\s@UpdateResource' {} a -> s {bookingOptions = a} :: UpdateResource)

-- | The identifier associated with the organization for which the resource
-- is updated.
updateResource_organizationId :: Lens.Lens' UpdateResource Prelude.Text
updateResource_organizationId = Lens.lens (\UpdateResource' {organizationId} -> organizationId) (\s@UpdateResource' {} a -> s {organizationId = a} :: UpdateResource)

-- | The identifier of the resource to be updated.
updateResource_resourceId :: Lens.Lens' UpdateResource Prelude.Text
updateResource_resourceId = Lens.lens (\UpdateResource' {resourceId} -> resourceId) (\s@UpdateResource' {} a -> s {resourceId = a} :: UpdateResource)

instance Prelude.AWSRequest UpdateResource where
  type Rs UpdateResource = UpdateResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResource

instance Prelude.NFData UpdateResource

instance Prelude.ToHeaders UpdateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.UpdateResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateResource where
  toJSON UpdateResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("BookingOptions" Prelude..=)
              Prelude.<$> bookingOptions,
            Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("ResourceId" Prelude..= resourceId)
          ]
      )

instance Prelude.ToPath UpdateResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceResponse' smart constructor.
data UpdateResourceResponse = UpdateResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateResourceResponse
