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
-- Module      : Amazonka.Lightsail.DeleteContainerImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a container image that is registered to your Amazon Lightsail
-- container service.
module Amazonka.Lightsail.DeleteContainerImage
  ( -- * Creating a Request
    DeleteContainerImage (..),
    newDeleteContainerImage,

    -- * Request Lenses
    deleteContainerImage_serviceName,
    deleteContainerImage_image,

    -- * Destructuring the Response
    DeleteContainerImageResponse (..),
    newDeleteContainerImageResponse,

    -- * Response Lenses
    deleteContainerImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContainerImage' smart constructor.
data DeleteContainerImage = DeleteContainerImage'
  { -- | The name of the container service for which to delete a registered
    -- container image.
    serviceName :: Prelude.Text,
    -- | The name of the container image to delete from the container service.
    --
    -- Use the @GetContainerImages@ action to get the name of the container
    -- images that are registered to a container service.
    --
    -- Container images sourced from your Lightsail container service, that are
    -- registered and stored on your service, start with a colon (@:@). For
    -- example, @:container-service-1.mystaticwebsite.1@. Container images
    -- sourced from a public registry like Docker Hub don\'t start with a
    -- colon. For example, @nginx:latest@ or @nginx@.
    image :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContainerImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'deleteContainerImage_serviceName' - The name of the container service for which to delete a registered
-- container image.
--
-- 'image', 'deleteContainerImage_image' - The name of the container image to delete from the container service.
--
-- Use the @GetContainerImages@ action to get the name of the container
-- images that are registered to a container service.
--
-- Container images sourced from your Lightsail container service, that are
-- registered and stored on your service, start with a colon (@:@). For
-- example, @:container-service-1.mystaticwebsite.1@. Container images
-- sourced from a public registry like Docker Hub don\'t start with a
-- colon. For example, @nginx:latest@ or @nginx@.
newDeleteContainerImage ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'image'
  Prelude.Text ->
  DeleteContainerImage
newDeleteContainerImage pServiceName_ pImage_ =
  DeleteContainerImage'
    { serviceName = pServiceName_,
      image = pImage_
    }

-- | The name of the container service for which to delete a registered
-- container image.
deleteContainerImage_serviceName :: Lens.Lens' DeleteContainerImage Prelude.Text
deleteContainerImage_serviceName = Lens.lens (\DeleteContainerImage' {serviceName} -> serviceName) (\s@DeleteContainerImage' {} a -> s {serviceName = a} :: DeleteContainerImage)

-- | The name of the container image to delete from the container service.
--
-- Use the @GetContainerImages@ action to get the name of the container
-- images that are registered to a container service.
--
-- Container images sourced from your Lightsail container service, that are
-- registered and stored on your service, start with a colon (@:@). For
-- example, @:container-service-1.mystaticwebsite.1@. Container images
-- sourced from a public registry like Docker Hub don\'t start with a
-- colon. For example, @nginx:latest@ or @nginx@.
deleteContainerImage_image :: Lens.Lens' DeleteContainerImage Prelude.Text
deleteContainerImage_image = Lens.lens (\DeleteContainerImage' {image} -> image) (\s@DeleteContainerImage' {} a -> s {image = a} :: DeleteContainerImage)

instance Core.AWSRequest DeleteContainerImage where
  type
    AWSResponse DeleteContainerImage =
      DeleteContainerImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerImageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContainerImage where
  hashWithSalt _salt DeleteContainerImage' {..} =
    _salt
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` image

instance Prelude.NFData DeleteContainerImage where
  rnf DeleteContainerImage' {..} =
    Prelude.rnf serviceName `Prelude.seq`
      Prelude.rnf image

instance Data.ToHeaders DeleteContainerImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteContainerImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteContainerImage where
  toJSON DeleteContainerImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("image" Data..= image)
          ]
      )

instance Data.ToPath DeleteContainerImage where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteContainerImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContainerImageResponse' smart constructor.
data DeleteContainerImageResponse = DeleteContainerImageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContainerImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContainerImageResponse_httpStatus' - The response's http status code.
newDeleteContainerImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContainerImageResponse
newDeleteContainerImageResponse pHttpStatus_ =
  DeleteContainerImageResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteContainerImageResponse_httpStatus :: Lens.Lens' DeleteContainerImageResponse Prelude.Int
deleteContainerImageResponse_httpStatus = Lens.lens (\DeleteContainerImageResponse' {httpStatus} -> httpStatus) (\s@DeleteContainerImageResponse' {} a -> s {httpStatus = a} :: DeleteContainerImageResponse)

instance Prelude.NFData DeleteContainerImageResponse where
  rnf DeleteContainerImageResponse' {..} =
    Prelude.rnf httpStatus
