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
-- Module      : Network.AWS.CodeBuild.RetryBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a build.
module Network.AWS.CodeBuild.RetryBuild
  ( -- * Creating a Request
    RetryBuild (..),
    newRetryBuild,

    -- * Request Lenses
    retryBuild_idempotencyToken,
    retryBuild_id,

    -- * Destructuring the Response
    RetryBuildResponse (..),
    newRetryBuildResponse,

    -- * Response Lenses
    retryBuildResponse_build,
    retryBuildResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRetryBuild' smart constructor.
data RetryBuild = RetryBuild'
  { -- | A unique, case sensitive identifier you provide to ensure the
    -- idempotency of the @RetryBuild@ request. The token is included in the
    -- @RetryBuild@ request and is valid for five minutes. If you repeat the
    -- @RetryBuild@ request with the same token, but change a parameter, AWS
    -- CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the identifier of the build to restart.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'retryBuild_idempotencyToken' - A unique, case sensitive identifier you provide to ensure the
-- idempotency of the @RetryBuild@ request. The token is included in the
-- @RetryBuild@ request and is valid for five minutes. If you repeat the
-- @RetryBuild@ request with the same token, but change a parameter, AWS
-- CodeBuild returns a parameter mismatch error.
--
-- 'id', 'retryBuild_id' - Specifies the identifier of the build to restart.
newRetryBuild ::
  RetryBuild
newRetryBuild =
  RetryBuild'
    { idempotencyToken = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | A unique, case sensitive identifier you provide to ensure the
-- idempotency of the @RetryBuild@ request. The token is included in the
-- @RetryBuild@ request and is valid for five minutes. If you repeat the
-- @RetryBuild@ request with the same token, but change a parameter, AWS
-- CodeBuild returns a parameter mismatch error.
retryBuild_idempotencyToken :: Lens.Lens' RetryBuild (Prelude.Maybe Prelude.Text)
retryBuild_idempotencyToken = Lens.lens (\RetryBuild' {idempotencyToken} -> idempotencyToken) (\s@RetryBuild' {} a -> s {idempotencyToken = a} :: RetryBuild)

-- | Specifies the identifier of the build to restart.
retryBuild_id :: Lens.Lens' RetryBuild (Prelude.Maybe Prelude.Text)
retryBuild_id = Lens.lens (\RetryBuild' {id} -> id) (\s@RetryBuild' {} a -> s {id = a} :: RetryBuild)

instance Core.AWSRequest RetryBuild where
  type AWSResponse RetryBuild = RetryBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryBuildResponse'
            Prelude.<$> (x Core..?> "build")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetryBuild

instance Prelude.NFData RetryBuild

instance Core.ToHeaders RetryBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.RetryBuild" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RetryBuild where
  toJSON RetryBuild' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("idempotencyToken" Core..=)
              Prelude.<$> idempotencyToken,
            ("id" Core..=) Prelude.<$> id
          ]
      )

instance Core.ToPath RetryBuild where
  toPath = Prelude.const "/"

instance Core.ToQuery RetryBuild where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRetryBuildResponse' smart constructor.
data RetryBuildResponse = RetryBuildResponse'
  { build :: Prelude.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryBuildResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'build', 'retryBuildResponse_build' - Undocumented member.
--
-- 'httpStatus', 'retryBuildResponse_httpStatus' - The response's http status code.
newRetryBuildResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetryBuildResponse
newRetryBuildResponse pHttpStatus_ =
  RetryBuildResponse'
    { build = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
retryBuildResponse_build :: Lens.Lens' RetryBuildResponse (Prelude.Maybe Build)
retryBuildResponse_build = Lens.lens (\RetryBuildResponse' {build} -> build) (\s@RetryBuildResponse' {} a -> s {build = a} :: RetryBuildResponse)

-- | The response's http status code.
retryBuildResponse_httpStatus :: Lens.Lens' RetryBuildResponse Prelude.Int
retryBuildResponse_httpStatus = Lens.lens (\RetryBuildResponse' {httpStatus} -> httpStatus) (\s@RetryBuildResponse' {} a -> s {httpStatus = a} :: RetryBuildResponse)

instance Prelude.NFData RetryBuildResponse
