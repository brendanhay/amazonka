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
-- Module      : Amazonka.CodeBuild.InvalidateProjectCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the cache for a project.
module Amazonka.CodeBuild.InvalidateProjectCache
  ( -- * Creating a Request
    InvalidateProjectCache (..),
    newInvalidateProjectCache,

    -- * Request Lenses
    invalidateProjectCache_projectName,

    -- * Destructuring the Response
    InvalidateProjectCacheResponse (..),
    newInvalidateProjectCacheResponse,

    -- * Response Lenses
    invalidateProjectCacheResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInvalidateProjectCache' smart constructor.
data InvalidateProjectCache = InvalidateProjectCache'
  { -- | The name of the CodeBuild build project that the cache is reset for.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvalidateProjectCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'invalidateProjectCache_projectName' - The name of the CodeBuild build project that the cache is reset for.
newInvalidateProjectCache ::
  -- | 'projectName'
  Prelude.Text ->
  InvalidateProjectCache
newInvalidateProjectCache pProjectName_ =
  InvalidateProjectCache'
    { projectName =
        pProjectName_
    }

-- | The name of the CodeBuild build project that the cache is reset for.
invalidateProjectCache_projectName :: Lens.Lens' InvalidateProjectCache Prelude.Text
invalidateProjectCache_projectName = Lens.lens (\InvalidateProjectCache' {projectName} -> projectName) (\s@InvalidateProjectCache' {} a -> s {projectName = a} :: InvalidateProjectCache)

instance Core.AWSRequest InvalidateProjectCache where
  type
    AWSResponse InvalidateProjectCache =
      InvalidateProjectCacheResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          InvalidateProjectCacheResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InvalidateProjectCache where
  hashWithSalt _salt InvalidateProjectCache' {..} =
    _salt `Prelude.hashWithSalt` projectName

instance Prelude.NFData InvalidateProjectCache where
  rnf InvalidateProjectCache' {..} =
    Prelude.rnf projectName

instance Data.ToHeaders InvalidateProjectCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.InvalidateProjectCache" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InvalidateProjectCache where
  toJSON InvalidateProjectCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("projectName" Data..= projectName)]
      )

instance Data.ToPath InvalidateProjectCache where
  toPath = Prelude.const "/"

instance Data.ToQuery InvalidateProjectCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInvalidateProjectCacheResponse' smart constructor.
data InvalidateProjectCacheResponse = InvalidateProjectCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvalidateProjectCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'invalidateProjectCacheResponse_httpStatus' - The response's http status code.
newInvalidateProjectCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InvalidateProjectCacheResponse
newInvalidateProjectCacheResponse pHttpStatus_ =
  InvalidateProjectCacheResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
invalidateProjectCacheResponse_httpStatus :: Lens.Lens' InvalidateProjectCacheResponse Prelude.Int
invalidateProjectCacheResponse_httpStatus = Lens.lens (\InvalidateProjectCacheResponse' {httpStatus} -> httpStatus) (\s@InvalidateProjectCacheResponse' {} a -> s {httpStatus = a} :: InvalidateProjectCacheResponse)

instance
  Prelude.NFData
    InvalidateProjectCacheResponse
  where
  rnf InvalidateProjectCacheResponse' {..} =
    Prelude.rnf httpStatus
