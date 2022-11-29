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
-- Module      : Amazonka.CodeBuild.StopBuild
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop running a build.
module Amazonka.CodeBuild.StopBuild
  ( -- * Creating a Request
    StopBuild (..),
    newStopBuild,

    -- * Request Lenses
    stopBuild_id,

    -- * Destructuring the Response
    StopBuildResponse (..),
    newStopBuildResponse,

    -- * Response Lenses
    stopBuildResponse_build,
    stopBuildResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopBuild' smart constructor.
data StopBuild = StopBuild'
  { -- | The ID of the build.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'stopBuild_id' - The ID of the build.
newStopBuild ::
  -- | 'id'
  Prelude.Text ->
  StopBuild
newStopBuild pId_ = StopBuild' {id = pId_}

-- | The ID of the build.
stopBuild_id :: Lens.Lens' StopBuild Prelude.Text
stopBuild_id = Lens.lens (\StopBuild' {id} -> id) (\s@StopBuild' {} a -> s {id = a} :: StopBuild)

instance Core.AWSRequest StopBuild where
  type AWSResponse StopBuild = StopBuildResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopBuildResponse'
            Prelude.<$> (x Core..?> "build")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopBuild where
  hashWithSalt _salt StopBuild' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData StopBuild where
  rnf StopBuild' {..} = Prelude.rnf id

instance Core.ToHeaders StopBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.StopBuild" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopBuild where
  toJSON StopBuild' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath StopBuild where
  toPath = Prelude.const "/"

instance Core.ToQuery StopBuild where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopBuildResponse' smart constructor.
data StopBuildResponse = StopBuildResponse'
  { -- | Information about the build.
    build :: Prelude.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopBuildResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'build', 'stopBuildResponse_build' - Information about the build.
--
-- 'httpStatus', 'stopBuildResponse_httpStatus' - The response's http status code.
newStopBuildResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopBuildResponse
newStopBuildResponse pHttpStatus_ =
  StopBuildResponse'
    { build = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the build.
stopBuildResponse_build :: Lens.Lens' StopBuildResponse (Prelude.Maybe Build)
stopBuildResponse_build = Lens.lens (\StopBuildResponse' {build} -> build) (\s@StopBuildResponse' {} a -> s {build = a} :: StopBuildResponse)

-- | The response's http status code.
stopBuildResponse_httpStatus :: Lens.Lens' StopBuildResponse Prelude.Int
stopBuildResponse_httpStatus = Lens.lens (\StopBuildResponse' {httpStatus} -> httpStatus) (\s@StopBuildResponse' {} a -> s {httpStatus = a} :: StopBuildResponse)

instance Prelude.NFData StopBuildResponse where
  rnf StopBuildResponse' {..} =
    Prelude.rnf build
      `Prelude.seq` Prelude.rnf httpStatus
