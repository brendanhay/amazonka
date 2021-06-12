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
-- Module      : Network.AWS.CodeBuild.BatchGetBuilds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more builds.
module Network.AWS.CodeBuild.BatchGetBuilds
  ( -- * Creating a Request
    BatchGetBuilds (..),
    newBatchGetBuilds,

    -- * Request Lenses
    batchGetBuilds_ids,

    -- * Destructuring the Response
    BatchGetBuildsResponse (..),
    newBatchGetBuildsResponse,

    -- * Response Lenses
    batchGetBuildsResponse_buildsNotFound,
    batchGetBuildsResponse_builds,
    batchGetBuildsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetBuilds' smart constructor.
data BatchGetBuilds = BatchGetBuilds'
  { -- | The IDs of the builds.
    ids :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetBuilds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'batchGetBuilds_ids' - The IDs of the builds.
newBatchGetBuilds ::
  -- | 'ids'
  Core.NonEmpty Core.Text ->
  BatchGetBuilds
newBatchGetBuilds pIds_ =
  BatchGetBuilds' {ids = Lens._Coerce Lens.# pIds_}

-- | The IDs of the builds.
batchGetBuilds_ids :: Lens.Lens' BatchGetBuilds (Core.NonEmpty Core.Text)
batchGetBuilds_ids = Lens.lens (\BatchGetBuilds' {ids} -> ids) (\s@BatchGetBuilds' {} a -> s {ids = a} :: BatchGetBuilds) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetBuilds where
  type
    AWSResponse BatchGetBuilds =
      BatchGetBuildsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBuildsResponse'
            Core.<$> (x Core..?> "buildsNotFound")
            Core.<*> (x Core..?> "builds" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetBuilds

instance Core.NFData BatchGetBuilds

instance Core.ToHeaders BatchGetBuilds where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.BatchGetBuilds" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetBuilds where
  toJSON BatchGetBuilds' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ids" Core..= ids)])

instance Core.ToPath BatchGetBuilds where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetBuilds where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetBuildsResponse' smart constructor.
data BatchGetBuildsResponse = BatchGetBuildsResponse'
  { -- | The IDs of builds for which information could not be found.
    buildsNotFound :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Information about the requested builds.
    builds :: Core.Maybe [Build],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetBuildsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildsNotFound', 'batchGetBuildsResponse_buildsNotFound' - The IDs of builds for which information could not be found.
--
-- 'builds', 'batchGetBuildsResponse_builds' - Information about the requested builds.
--
-- 'httpStatus', 'batchGetBuildsResponse_httpStatus' - The response's http status code.
newBatchGetBuildsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetBuildsResponse
newBatchGetBuildsResponse pHttpStatus_ =
  BatchGetBuildsResponse'
    { buildsNotFound =
        Core.Nothing,
      builds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of builds for which information could not be found.
batchGetBuildsResponse_buildsNotFound :: Lens.Lens' BatchGetBuildsResponse (Core.Maybe (Core.NonEmpty Core.Text))
batchGetBuildsResponse_buildsNotFound = Lens.lens (\BatchGetBuildsResponse' {buildsNotFound} -> buildsNotFound) (\s@BatchGetBuildsResponse' {} a -> s {buildsNotFound = a} :: BatchGetBuildsResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the requested builds.
batchGetBuildsResponse_builds :: Lens.Lens' BatchGetBuildsResponse (Core.Maybe [Build])
batchGetBuildsResponse_builds = Lens.lens (\BatchGetBuildsResponse' {builds} -> builds) (\s@BatchGetBuildsResponse' {} a -> s {builds = a} :: BatchGetBuildsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetBuildsResponse_httpStatus :: Lens.Lens' BatchGetBuildsResponse Core.Int
batchGetBuildsResponse_httpStatus = Lens.lens (\BatchGetBuildsResponse' {httpStatus} -> httpStatus) (\s@BatchGetBuildsResponse' {} a -> s {httpStatus = a} :: BatchGetBuildsResponse)

instance Core.NFData BatchGetBuildsResponse
