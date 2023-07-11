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
-- Module      : Amazonka.GameLift.UpdateBuild
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.UpdateBuild
  ( -- * Creating a Request
    UpdateBuild (..),
    newUpdateBuild,

    -- * Request Lenses
    updateBuild_name,
    updateBuild_version,
    updateBuild_buildId,

    -- * Destructuring the Response
    UpdateBuildResponse (..),
    newUpdateBuildResponse,

    -- * Response Lenses
    updateBuildResponse_build,
    updateBuildResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBuild' smart constructor.
data UpdateBuild = UpdateBuild'
  { -- | A descriptive label associated with a build. Build names do not need to
    -- be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Version information associated with a build or script. Version strings
    -- do not need to be unique.
    version :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the build to update. You can use either the
    -- build ID or ARN value.
    buildId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateBuild_name' - A descriptive label associated with a build. Build names do not need to
-- be unique.
--
-- 'version', 'updateBuild_version' - Version information associated with a build or script. Version strings
-- do not need to be unique.
--
-- 'buildId', 'updateBuild_buildId' - A unique identifier for the build to update. You can use either the
-- build ID or ARN value.
newUpdateBuild ::
  -- | 'buildId'
  Prelude.Text ->
  UpdateBuild
newUpdateBuild pBuildId_ =
  UpdateBuild'
    { name = Prelude.Nothing,
      version = Prelude.Nothing,
      buildId = pBuildId_
    }

-- | A descriptive label associated with a build. Build names do not need to
-- be unique.
updateBuild_name :: Lens.Lens' UpdateBuild (Prelude.Maybe Prelude.Text)
updateBuild_name = Lens.lens (\UpdateBuild' {name} -> name) (\s@UpdateBuild' {} a -> s {name = a} :: UpdateBuild)

-- | Version information associated with a build or script. Version strings
-- do not need to be unique.
updateBuild_version :: Lens.Lens' UpdateBuild (Prelude.Maybe Prelude.Text)
updateBuild_version = Lens.lens (\UpdateBuild' {version} -> version) (\s@UpdateBuild' {} a -> s {version = a} :: UpdateBuild)

-- | A unique identifier for the build to update. You can use either the
-- build ID or ARN value.
updateBuild_buildId :: Lens.Lens' UpdateBuild Prelude.Text
updateBuild_buildId = Lens.lens (\UpdateBuild' {buildId} -> buildId) (\s@UpdateBuild' {} a -> s {buildId = a} :: UpdateBuild)

instance Core.AWSRequest UpdateBuild where
  type AWSResponse UpdateBuild = UpdateBuildResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBuildResponse'
            Prelude.<$> (x Data..?> "Build")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBuild where
  hashWithSalt _salt UpdateBuild' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` buildId

instance Prelude.NFData UpdateBuild where
  rnf UpdateBuild' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf buildId

instance Data.ToHeaders UpdateBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.UpdateBuild" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBuild where
  toJSON UpdateBuild' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Version" Data..=) Prelude.<$> version,
            Prelude.Just ("BuildId" Data..= buildId)
          ]
      )

instance Data.ToPath UpdateBuild where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateBuild where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBuildResponse' smart constructor.
data UpdateBuildResponse = UpdateBuildResponse'
  { -- | The updated build resource.
    build :: Prelude.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateBuildResponse where
  rnf UpdateBuildResponse' {..} =
    Prelude.rnf build
      `Prelude.seq` Prelude.rnf httpStatus
