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
-- Module      : Network.AWS.GameLift.UpdateBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata in a build resource, including the build name and
-- version. To update the metadata, specify the build ID to update and
-- provide the new values. If successful, a build object containing the
-- updated metadata is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
--
-- __Related operations__
--
-- -   CreateBuild
--
-- -   ListBuilds
--
-- -   DescribeBuild
--
-- -   UpdateBuild
--
-- -   DeleteBuild
module Network.AWS.GameLift.UpdateBuild
  ( -- * Creating a Request
    UpdateBuild (..),
    newUpdateBuild,

    -- * Request Lenses
    updateBuild_version,
    updateBuild_name,
    updateBuild_buildId,

    -- * Destructuring the Response
    UpdateBuildResponse (..),
    newUpdateBuildResponse,

    -- * Response Lenses
    updateBuildResponse_build,
    updateBuildResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateBuild' smart constructor.
data UpdateBuild = UpdateBuild'
  { -- | Version information that is associated with a build or script. Version
    -- strings do not need to be unique.
    version :: Core.Maybe Core.Text,
    -- | A descriptive label that is associated with a build. Build names do not
    -- need to be unique.
    name :: Core.Maybe Core.Text,
    -- | A unique identifier for a build to update. You can use either the build
    -- ID or ARN value.
    buildId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'updateBuild_version' - Version information that is associated with a build or script. Version
-- strings do not need to be unique.
--
-- 'name', 'updateBuild_name' - A descriptive label that is associated with a build. Build names do not
-- need to be unique.
--
-- 'buildId', 'updateBuild_buildId' - A unique identifier for a build to update. You can use either the build
-- ID or ARN value.
newUpdateBuild ::
  -- | 'buildId'
  Core.Text ->
  UpdateBuild
newUpdateBuild pBuildId_ =
  UpdateBuild'
    { version = Core.Nothing,
      name = Core.Nothing,
      buildId = pBuildId_
    }

-- | Version information that is associated with a build or script. Version
-- strings do not need to be unique.
updateBuild_version :: Lens.Lens' UpdateBuild (Core.Maybe Core.Text)
updateBuild_version = Lens.lens (\UpdateBuild' {version} -> version) (\s@UpdateBuild' {} a -> s {version = a} :: UpdateBuild)

-- | A descriptive label that is associated with a build. Build names do not
-- need to be unique.
updateBuild_name :: Lens.Lens' UpdateBuild (Core.Maybe Core.Text)
updateBuild_name = Lens.lens (\UpdateBuild' {name} -> name) (\s@UpdateBuild' {} a -> s {name = a} :: UpdateBuild)

-- | A unique identifier for a build to update. You can use either the build
-- ID or ARN value.
updateBuild_buildId :: Lens.Lens' UpdateBuild Core.Text
updateBuild_buildId = Lens.lens (\UpdateBuild' {buildId} -> buildId) (\s@UpdateBuild' {} a -> s {buildId = a} :: UpdateBuild)

instance Core.AWSRequest UpdateBuild where
  type AWSResponse UpdateBuild = UpdateBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBuildResponse'
            Core.<$> (x Core..?> "Build")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateBuild

instance Core.NFData UpdateBuild

instance Core.ToHeaders UpdateBuild where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.UpdateBuild" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBuild where
  toJSON UpdateBuild' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Version" Core..=) Core.<$> version,
            ("Name" Core..=) Core.<$> name,
            Core.Just ("BuildId" Core..= buildId)
          ]
      )

instance Core.ToPath UpdateBuild where
  toPath = Core.const "/"

instance Core.ToQuery UpdateBuild where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateBuildResponse' smart constructor.
data UpdateBuildResponse = UpdateBuildResponse'
  { -- | The updated build resource.
    build :: Core.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBuildResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'build', 'updateBuildResponse_build' - The updated build resource.
--
-- 'httpStatus', 'updateBuildResponse_httpStatus' - The response's http status code.
newUpdateBuildResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateBuildResponse
newUpdateBuildResponse pHttpStatus_ =
  UpdateBuildResponse'
    { build = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated build resource.
updateBuildResponse_build :: Lens.Lens' UpdateBuildResponse (Core.Maybe Build)
updateBuildResponse_build = Lens.lens (\UpdateBuildResponse' {build} -> build) (\s@UpdateBuildResponse' {} a -> s {build = a} :: UpdateBuildResponse)

-- | The response's http status code.
updateBuildResponse_httpStatus :: Lens.Lens' UpdateBuildResponse Core.Int
updateBuildResponse_httpStatus = Lens.lens (\UpdateBuildResponse' {httpStatus} -> httpStatus) (\s@UpdateBuildResponse' {} a -> s {httpStatus = a} :: UpdateBuildResponse)

instance Core.NFData UpdateBuildResponse
