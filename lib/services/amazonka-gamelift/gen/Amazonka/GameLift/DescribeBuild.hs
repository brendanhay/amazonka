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
-- Module      : Amazonka.GameLift.DescribeBuild
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DescribeBuild
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBuild' smart constructor.
data DescribeBuild = DescribeBuild'
  { -- | A unique identifier for the build to retrieve properties for. You can
    -- use either the build ID or ARN value.
    buildId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildId', 'describeBuild_buildId' - A unique identifier for the build to retrieve properties for. You can
-- use either the build ID or ARN value.
newDescribeBuild ::
  -- | 'buildId'
  Prelude.Text ->
  DescribeBuild
newDescribeBuild pBuildId_ =
  DescribeBuild' {buildId = pBuildId_}

-- | A unique identifier for the build to retrieve properties for. You can
-- use either the build ID or ARN value.
describeBuild_buildId :: Lens.Lens' DescribeBuild Prelude.Text
describeBuild_buildId = Lens.lens (\DescribeBuild' {buildId} -> buildId) (\s@DescribeBuild' {} a -> s {buildId = a} :: DescribeBuild)

instance Core.AWSRequest DescribeBuild where
  type
    AWSResponse DescribeBuild =
      DescribeBuildResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBuildResponse'
            Prelude.<$> (x Data..?> "Build")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBuild where
  hashWithSalt _salt DescribeBuild' {..} =
    _salt `Prelude.hashWithSalt` buildId

instance Prelude.NFData DescribeBuild where
  rnf DescribeBuild' {..} = Prelude.rnf buildId

instance Data.ToHeaders DescribeBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DescribeBuild" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBuild where
  toJSON DescribeBuild' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BuildId" Data..= buildId)]
      )

instance Data.ToPath DescribeBuild where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBuild where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBuildResponse' smart constructor.
data DescribeBuildResponse = DescribeBuildResponse'
  { -- | Set of properties describing the requested build.
    build :: Prelude.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DescribeBuildResponse where
  rnf DescribeBuildResponse' {..} =
    Prelude.rnf build
      `Prelude.seq` Prelude.rnf httpStatus
