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
-- Module      : Network.AWS.GameLift.DescribeBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a custom game build. To request a build
-- resource, specify a build ID. If successful, an object containing the
-- build properties is returned.
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
module Network.AWS.GameLift.DescribeBuild
  ( -- * Creating a Request
    DescribeBuild (..),
    newDescribeBuild,

    -- * Request Lenses
    describeBuild_buildId,

    -- * Destructuring the Response
    DescribeBuildResponse (..),
    newDescribeBuildResponse,

    -- * Response Lenses
    describeBuildResponse_build,
    describeBuildResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeBuild' smart constructor.
data DescribeBuild = DescribeBuild'
  { -- | A unique identifier for a build to retrieve properties for. You can use
    -- either the build ID or ARN value.
    buildId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildId', 'describeBuild_buildId' - A unique identifier for a build to retrieve properties for. You can use
-- either the build ID or ARN value.
newDescribeBuild ::
  -- | 'buildId'
  Core.Text ->
  DescribeBuild
newDescribeBuild pBuildId_ =
  DescribeBuild' {buildId = pBuildId_}

-- | A unique identifier for a build to retrieve properties for. You can use
-- either the build ID or ARN value.
describeBuild_buildId :: Lens.Lens' DescribeBuild Core.Text
describeBuild_buildId = Lens.lens (\DescribeBuild' {buildId} -> buildId) (\s@DescribeBuild' {} a -> s {buildId = a} :: DescribeBuild)

instance Core.AWSRequest DescribeBuild where
  type
    AWSResponse DescribeBuild =
      DescribeBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBuildResponse'
            Core.<$> (x Core..?> "Build")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeBuild

instance Core.NFData DescribeBuild

instance Core.ToHeaders DescribeBuild where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DescribeBuild" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeBuild where
  toJSON DescribeBuild' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("BuildId" Core..= buildId)]
      )

instance Core.ToPath DescribeBuild where
  toPath = Core.const "/"

instance Core.ToQuery DescribeBuild where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeBuildResponse' smart constructor.
data DescribeBuildResponse = DescribeBuildResponse'
  { -- | Set of properties describing the requested build.
    build :: Core.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBuildResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'build', 'describeBuildResponse_build' - Set of properties describing the requested build.
--
-- 'httpStatus', 'describeBuildResponse_httpStatus' - The response's http status code.
newDescribeBuildResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeBuildResponse
newDescribeBuildResponse pHttpStatus_ =
  DescribeBuildResponse'
    { build = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Set of properties describing the requested build.
describeBuildResponse_build :: Lens.Lens' DescribeBuildResponse (Core.Maybe Build)
describeBuildResponse_build = Lens.lens (\DescribeBuildResponse' {build} -> build) (\s@DescribeBuildResponse' {} a -> s {build = a} :: DescribeBuildResponse)

-- | The response's http status code.
describeBuildResponse_httpStatus :: Lens.Lens' DescribeBuildResponse Core.Int
describeBuildResponse_httpStatus = Lens.lens (\DescribeBuildResponse' {httpStatus} -> httpStatus) (\s@DescribeBuildResponse' {} a -> s {httpStatus = a} :: DescribeBuildResponse)

instance Core.NFData DescribeBuildResponse
