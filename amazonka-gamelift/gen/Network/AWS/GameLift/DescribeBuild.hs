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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeBuild' smart constructor.
data DescribeBuild = DescribeBuild'
  { -- | A unique identifier for a build to retrieve properties for. You can use
    -- either the build ID or ARN value.
    buildId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeBuild
newDescribeBuild pBuildId_ =
  DescribeBuild' {buildId = pBuildId_}

-- | A unique identifier for a build to retrieve properties for. You can use
-- either the build ID or ARN value.
describeBuild_buildId :: Lens.Lens' DescribeBuild Prelude.Text
describeBuild_buildId = Lens.lens (\DescribeBuild' {buildId} -> buildId) (\s@DescribeBuild' {} a -> s {buildId = a} :: DescribeBuild)

instance Prelude.AWSRequest DescribeBuild where
  type Rs DescribeBuild = DescribeBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBuildResponse'
            Prelude.<$> (x Prelude..?> "Build")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBuild

instance Prelude.NFData DescribeBuild

instance Prelude.ToHeaders DescribeBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.DescribeBuild" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeBuild where
  toJSON DescribeBuild' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("BuildId" Prelude..= buildId)]
      )

instance Prelude.ToPath DescribeBuild where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeBuild where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeBuildResponse' smart constructor.
data DescribeBuildResponse = DescribeBuildResponse'
  { -- | Set of properties describing the requested build.
    build :: Prelude.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeBuildResponse
newDescribeBuildResponse pHttpStatus_ =
  DescribeBuildResponse'
    { build = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Set of properties describing the requested build.
describeBuildResponse_build :: Lens.Lens' DescribeBuildResponse (Prelude.Maybe Build)
describeBuildResponse_build = Lens.lens (\DescribeBuildResponse' {build} -> build) (\s@DescribeBuildResponse' {} a -> s {build = a} :: DescribeBuildResponse)

-- | The response's http status code.
describeBuildResponse_httpStatus :: Lens.Lens' DescribeBuildResponse Prelude.Int
describeBuildResponse_httpStatus = Lens.lens (\DescribeBuildResponse' {httpStatus} -> httpStatus) (\s@DescribeBuildResponse' {} a -> s {httpStatus = a} :: DescribeBuildResponse)

instance Prelude.NFData DescribeBuildResponse
