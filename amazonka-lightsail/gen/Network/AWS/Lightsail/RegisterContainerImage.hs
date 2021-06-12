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
-- Module      : Network.AWS.Lightsail.RegisterContainerImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a container image to your Amazon Lightsail container service.
--
-- This action is not required if you install and use the Lightsail Control
-- (lightsailctl) plugin to push container images to your Lightsail
-- container service. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-pushing-container-images Pushing and managing container images on your Amazon Lightsail container services>
-- in the /Lightsail Dev Guide/.
module Network.AWS.Lightsail.RegisterContainerImage
  ( -- * Creating a Request
    RegisterContainerImage (..),
    newRegisterContainerImage,

    -- * Request Lenses
    registerContainerImage_serviceName,
    registerContainerImage_label,
    registerContainerImage_digest,

    -- * Destructuring the Response
    RegisterContainerImageResponse (..),
    newRegisterContainerImageResponse,

    -- * Response Lenses
    registerContainerImageResponse_containerImage,
    registerContainerImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterContainerImage' smart constructor.
data RegisterContainerImage = RegisterContainerImage'
  { -- | The name of the container service for which to register a container
    -- image.
    serviceName :: Core.Text,
    -- | The label for the container image when it\'s registered to the container
    -- service.
    --
    -- Use a descriptive label that you can use to track the different versions
    -- of your registered container images.
    --
    -- Use the @GetContainerImages@ action to return the container images
    -- registered to a Lightsail container service. The label is the
    -- @\<imagelabel>@ portion of the following image name example:
    --
    -- -   @:container-service-1.\<imagelabel>.1@
    --
    -- If the name of your container service is @mycontainerservice@, and the
    -- label that you specify is @mystaticwebsite@, then the name of the
    -- registered container image will be
    -- @:mycontainerservice.mystaticwebsite.1@.
    --
    -- The number at the end of these image name examples represents the
    -- version of the registered container image. If you push and register
    -- another container image to the same Lightsail container service, with
    -- the same label, then the version number for the new registered container
    -- image will be @2@. If you push and register another container image, the
    -- version number will be @3@, and so on.
    label :: Core.Text,
    -- | The digest of the container image to be registered.
    digest :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterContainerImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'registerContainerImage_serviceName' - The name of the container service for which to register a container
-- image.
--
-- 'label', 'registerContainerImage_label' - The label for the container image when it\'s registered to the container
-- service.
--
-- Use a descriptive label that you can use to track the different versions
-- of your registered container images.
--
-- Use the @GetContainerImages@ action to return the container images
-- registered to a Lightsail container service. The label is the
-- @\<imagelabel>@ portion of the following image name example:
--
-- -   @:container-service-1.\<imagelabel>.1@
--
-- If the name of your container service is @mycontainerservice@, and the
-- label that you specify is @mystaticwebsite@, then the name of the
-- registered container image will be
-- @:mycontainerservice.mystaticwebsite.1@.
--
-- The number at the end of these image name examples represents the
-- version of the registered container image. If you push and register
-- another container image to the same Lightsail container service, with
-- the same label, then the version number for the new registered container
-- image will be @2@. If you push and register another container image, the
-- version number will be @3@, and so on.
--
-- 'digest', 'registerContainerImage_digest' - The digest of the container image to be registered.
newRegisterContainerImage ::
  -- | 'serviceName'
  Core.Text ->
  -- | 'label'
  Core.Text ->
  -- | 'digest'
  Core.Text ->
  RegisterContainerImage
newRegisterContainerImage
  pServiceName_
  pLabel_
  pDigest_ =
    RegisterContainerImage'
      { serviceName =
          pServiceName_,
        label = pLabel_,
        digest = pDigest_
      }

-- | The name of the container service for which to register a container
-- image.
registerContainerImage_serviceName :: Lens.Lens' RegisterContainerImage Core.Text
registerContainerImage_serviceName = Lens.lens (\RegisterContainerImage' {serviceName} -> serviceName) (\s@RegisterContainerImage' {} a -> s {serviceName = a} :: RegisterContainerImage)

-- | The label for the container image when it\'s registered to the container
-- service.
--
-- Use a descriptive label that you can use to track the different versions
-- of your registered container images.
--
-- Use the @GetContainerImages@ action to return the container images
-- registered to a Lightsail container service. The label is the
-- @\<imagelabel>@ portion of the following image name example:
--
-- -   @:container-service-1.\<imagelabel>.1@
--
-- If the name of your container service is @mycontainerservice@, and the
-- label that you specify is @mystaticwebsite@, then the name of the
-- registered container image will be
-- @:mycontainerservice.mystaticwebsite.1@.
--
-- The number at the end of these image name examples represents the
-- version of the registered container image. If you push and register
-- another container image to the same Lightsail container service, with
-- the same label, then the version number for the new registered container
-- image will be @2@. If you push and register another container image, the
-- version number will be @3@, and so on.
registerContainerImage_label :: Lens.Lens' RegisterContainerImage Core.Text
registerContainerImage_label = Lens.lens (\RegisterContainerImage' {label} -> label) (\s@RegisterContainerImage' {} a -> s {label = a} :: RegisterContainerImage)

-- | The digest of the container image to be registered.
registerContainerImage_digest :: Lens.Lens' RegisterContainerImage Core.Text
registerContainerImage_digest = Lens.lens (\RegisterContainerImage' {digest} -> digest) (\s@RegisterContainerImage' {} a -> s {digest = a} :: RegisterContainerImage)

instance Core.AWSRequest RegisterContainerImage where
  type
    AWSResponse RegisterContainerImage =
      RegisterContainerImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterContainerImageResponse'
            Core.<$> (x Core..?> "containerImage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterContainerImage

instance Core.NFData RegisterContainerImage

instance Core.ToHeaders RegisterContainerImage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.RegisterContainerImage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterContainerImage where
  toJSON RegisterContainerImage' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("serviceName" Core..= serviceName),
            Core.Just ("label" Core..= label),
            Core.Just ("digest" Core..= digest)
          ]
      )

instance Core.ToPath RegisterContainerImage where
  toPath = Core.const "/"

instance Core.ToQuery RegisterContainerImage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterContainerImageResponse' smart constructor.
data RegisterContainerImageResponse = RegisterContainerImageResponse'
  { containerImage :: Core.Maybe ContainerImage,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterContainerImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerImage', 'registerContainerImageResponse_containerImage' - Undocumented member.
--
-- 'httpStatus', 'registerContainerImageResponse_httpStatus' - The response's http status code.
newRegisterContainerImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterContainerImageResponse
newRegisterContainerImageResponse pHttpStatus_ =
  RegisterContainerImageResponse'
    { containerImage =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
registerContainerImageResponse_containerImage :: Lens.Lens' RegisterContainerImageResponse (Core.Maybe ContainerImage)
registerContainerImageResponse_containerImage = Lens.lens (\RegisterContainerImageResponse' {containerImage} -> containerImage) (\s@RegisterContainerImageResponse' {} a -> s {containerImage = a} :: RegisterContainerImageResponse)

-- | The response's http status code.
registerContainerImageResponse_httpStatus :: Lens.Lens' RegisterContainerImageResponse Core.Int
registerContainerImageResponse_httpStatus = Lens.lens (\RegisterContainerImageResponse' {httpStatus} -> httpStatus) (\s@RegisterContainerImageResponse' {} a -> s {httpStatus = a} :: RegisterContainerImageResponse)

instance Core.NFData RegisterContainerImageResponse
