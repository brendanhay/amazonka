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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateBuild' smart constructor.
data UpdateBuild = UpdateBuild'
  { -- | Version information that is associated with a build or script. Version
    -- strings do not need to be unique.
    version :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a build. Build names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a build to update. You can use either the build
    -- ID or ARN value.
    buildId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateBuild
newUpdateBuild pBuildId_ =
  UpdateBuild'
    { version = Prelude.Nothing,
      name = Prelude.Nothing,
      buildId = pBuildId_
    }

-- | Version information that is associated with a build or script. Version
-- strings do not need to be unique.
updateBuild_version :: Lens.Lens' UpdateBuild (Prelude.Maybe Prelude.Text)
updateBuild_version = Lens.lens (\UpdateBuild' {version} -> version) (\s@UpdateBuild' {} a -> s {version = a} :: UpdateBuild)

-- | A descriptive label that is associated with a build. Build names do not
-- need to be unique.
updateBuild_name :: Lens.Lens' UpdateBuild (Prelude.Maybe Prelude.Text)
updateBuild_name = Lens.lens (\UpdateBuild' {name} -> name) (\s@UpdateBuild' {} a -> s {name = a} :: UpdateBuild)

-- | A unique identifier for a build to update. You can use either the build
-- ID or ARN value.
updateBuild_buildId :: Lens.Lens' UpdateBuild Prelude.Text
updateBuild_buildId = Lens.lens (\UpdateBuild' {buildId} -> buildId) (\s@UpdateBuild' {} a -> s {buildId = a} :: UpdateBuild)

instance Prelude.AWSRequest UpdateBuild where
  type Rs UpdateBuild = UpdateBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBuildResponse'
            Prelude.<$> (x Prelude..?> "Build")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBuild

instance Prelude.NFData UpdateBuild

instance Prelude.ToHeaders UpdateBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.UpdateBuild" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateBuild where
  toJSON UpdateBuild' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Version" Prelude..=) Prelude.<$> version,
            ("Name" Prelude..=) Prelude.<$> name,
            Prelude.Just ("BuildId" Prelude..= buildId)
          ]
      )

instance Prelude.ToPath UpdateBuild where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateBuild where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateBuildResponse' smart constructor.
data UpdateBuildResponse = UpdateBuildResponse'
  { -- | The updated build resource.
    build :: Prelude.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateBuildResponse
newUpdateBuildResponse pHttpStatus_ =
  UpdateBuildResponse'
    { build = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated build resource.
updateBuildResponse_build :: Lens.Lens' UpdateBuildResponse (Prelude.Maybe Build)
updateBuildResponse_build = Lens.lens (\UpdateBuildResponse' {build} -> build) (\s@UpdateBuildResponse' {} a -> s {build = a} :: UpdateBuildResponse)

-- | The response's http status code.
updateBuildResponse_httpStatus :: Lens.Lens' UpdateBuildResponse Prelude.Int
updateBuildResponse_httpStatus = Lens.lens (\UpdateBuildResponse' {httpStatus} -> httpStatus) (\s@UpdateBuildResponse' {} a -> s {httpStatus = a} :: UpdateBuildResponse)

instance Prelude.NFData UpdateBuildResponse
