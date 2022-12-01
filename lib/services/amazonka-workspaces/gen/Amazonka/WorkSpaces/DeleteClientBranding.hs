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
-- Module      : Amazonka.WorkSpaces.DeleteClientBranding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes customized client branding. Client branding allows you to
-- customize your WorkSpace\'s client login portal. You can tailor your
-- login portal company logo, the support email address, support link, link
-- to reset password, and a custom message for users trying to sign in.
--
-- After you delete your customized client branding, your login portal
-- reverts to the default client branding.
module Amazonka.WorkSpaces.DeleteClientBranding
  ( -- * Creating a Request
    DeleteClientBranding (..),
    newDeleteClientBranding,

    -- * Request Lenses
    deleteClientBranding_resourceId,
    deleteClientBranding_platforms,

    -- * Destructuring the Response
    DeleteClientBrandingResponse (..),
    newDeleteClientBrandingResponse,

    -- * Response Lenses
    deleteClientBrandingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDeleteClientBranding' smart constructor.
data DeleteClientBranding = DeleteClientBranding'
  { -- | The directory identifier of the WorkSpace for which you want to delete
    -- client branding.
    resourceId :: Prelude.Text,
    -- | The device type for which you want to delete client branding.
    platforms :: Prelude.NonEmpty ClientDeviceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientBranding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'deleteClientBranding_resourceId' - The directory identifier of the WorkSpace for which you want to delete
-- client branding.
--
-- 'platforms', 'deleteClientBranding_platforms' - The device type for which you want to delete client branding.
newDeleteClientBranding ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'platforms'
  Prelude.NonEmpty ClientDeviceType ->
  DeleteClientBranding
newDeleteClientBranding pResourceId_ pPlatforms_ =
  DeleteClientBranding'
    { resourceId = pResourceId_,
      platforms = Lens.coerced Lens.# pPlatforms_
    }

-- | The directory identifier of the WorkSpace for which you want to delete
-- client branding.
deleteClientBranding_resourceId :: Lens.Lens' DeleteClientBranding Prelude.Text
deleteClientBranding_resourceId = Lens.lens (\DeleteClientBranding' {resourceId} -> resourceId) (\s@DeleteClientBranding' {} a -> s {resourceId = a} :: DeleteClientBranding)

-- | The device type for which you want to delete client branding.
deleteClientBranding_platforms :: Lens.Lens' DeleteClientBranding (Prelude.NonEmpty ClientDeviceType)
deleteClientBranding_platforms = Lens.lens (\DeleteClientBranding' {platforms} -> platforms) (\s@DeleteClientBranding' {} a -> s {platforms = a} :: DeleteClientBranding) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteClientBranding where
  type
    AWSResponse DeleteClientBranding =
      DeleteClientBrandingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteClientBrandingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteClientBranding where
  hashWithSalt _salt DeleteClientBranding' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` platforms

instance Prelude.NFData DeleteClientBranding where
  rnf DeleteClientBranding' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf platforms

instance Core.ToHeaders DeleteClientBranding where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DeleteClientBranding" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteClientBranding where
  toJSON DeleteClientBranding' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just ("Platforms" Core..= platforms)
          ]
      )

instance Core.ToPath DeleteClientBranding where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteClientBranding where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClientBrandingResponse' smart constructor.
data DeleteClientBrandingResponse = DeleteClientBrandingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientBrandingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteClientBrandingResponse_httpStatus' - The response's http status code.
newDeleteClientBrandingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClientBrandingResponse
newDeleteClientBrandingResponse pHttpStatus_ =
  DeleteClientBrandingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteClientBrandingResponse_httpStatus :: Lens.Lens' DeleteClientBrandingResponse Prelude.Int
deleteClientBrandingResponse_httpStatus = Lens.lens (\DeleteClientBrandingResponse' {httpStatus} -> httpStatus) (\s@DeleteClientBrandingResponse' {} a -> s {httpStatus = a} :: DeleteClientBrandingResponse)

instance Prelude.NFData DeleteClientBrandingResponse where
  rnf DeleteClientBrandingResponse' {..} =
    Prelude.rnf httpStatus
