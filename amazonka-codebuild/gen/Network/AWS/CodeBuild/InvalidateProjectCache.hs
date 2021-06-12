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
-- Module      : Network.AWS.CodeBuild.InvalidateProjectCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the cache for a project.
module Network.AWS.CodeBuild.InvalidateProjectCache
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

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newInvalidateProjectCache' smart constructor.
data InvalidateProjectCache = InvalidateProjectCache'
  { -- | The name of the AWS CodeBuild build project that the cache is reset for.
    projectName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InvalidateProjectCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'invalidateProjectCache_projectName' - The name of the AWS CodeBuild build project that the cache is reset for.
newInvalidateProjectCache ::
  -- | 'projectName'
  Core.Text ->
  InvalidateProjectCache
newInvalidateProjectCache pProjectName_ =
  InvalidateProjectCache'
    { projectName =
        pProjectName_
    }

-- | The name of the AWS CodeBuild build project that the cache is reset for.
invalidateProjectCache_projectName :: Lens.Lens' InvalidateProjectCache Core.Text
invalidateProjectCache_projectName = Lens.lens (\InvalidateProjectCache' {projectName} -> projectName) (\s@InvalidateProjectCache' {} a -> s {projectName = a} :: InvalidateProjectCache)

instance Core.AWSRequest InvalidateProjectCache where
  type
    AWSResponse InvalidateProjectCache =
      InvalidateProjectCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          InvalidateProjectCacheResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable InvalidateProjectCache

instance Core.NFData InvalidateProjectCache

instance Core.ToHeaders InvalidateProjectCache where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.InvalidateProjectCache" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON InvalidateProjectCache where
  toJSON InvalidateProjectCache' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("projectName" Core..= projectName)]
      )

instance Core.ToPath InvalidateProjectCache where
  toPath = Core.const "/"

instance Core.ToQuery InvalidateProjectCache where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newInvalidateProjectCacheResponse' smart constructor.
data InvalidateProjectCacheResponse = InvalidateProjectCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  InvalidateProjectCacheResponse
newInvalidateProjectCacheResponse pHttpStatus_ =
  InvalidateProjectCacheResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
invalidateProjectCacheResponse_httpStatus :: Lens.Lens' InvalidateProjectCacheResponse Core.Int
invalidateProjectCacheResponse_httpStatus = Lens.lens (\InvalidateProjectCacheResponse' {httpStatus} -> httpStatus) (\s@InvalidateProjectCacheResponse' {} a -> s {httpStatus = a} :: InvalidateProjectCacheResponse)

instance Core.NFData InvalidateProjectCacheResponse
