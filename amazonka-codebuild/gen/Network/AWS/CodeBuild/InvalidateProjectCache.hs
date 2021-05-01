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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newInvalidateProjectCache' smart constructor.
data InvalidateProjectCache = InvalidateProjectCache'
  { -- | The name of the AWS CodeBuild build project that the cache is reset for.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  InvalidateProjectCache
newInvalidateProjectCache pProjectName_ =
  InvalidateProjectCache'
    { projectName =
        pProjectName_
    }

-- | The name of the AWS CodeBuild build project that the cache is reset for.
invalidateProjectCache_projectName :: Lens.Lens' InvalidateProjectCache Prelude.Text
invalidateProjectCache_projectName = Lens.lens (\InvalidateProjectCache' {projectName} -> projectName) (\s@InvalidateProjectCache' {} a -> s {projectName = a} :: InvalidateProjectCache)

instance Prelude.AWSRequest InvalidateProjectCache where
  type
    Rs InvalidateProjectCache =
      InvalidateProjectCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          InvalidateProjectCacheResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InvalidateProjectCache

instance Prelude.NFData InvalidateProjectCache

instance Prelude.ToHeaders InvalidateProjectCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.InvalidateProjectCache" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON InvalidateProjectCache where
  toJSON InvalidateProjectCache' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("projectName" Prelude..= projectName)
          ]
      )

instance Prelude.ToPath InvalidateProjectCache where
  toPath = Prelude.const "/"

instance Prelude.ToQuery InvalidateProjectCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInvalidateProjectCacheResponse' smart constructor.
data InvalidateProjectCacheResponse = InvalidateProjectCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
