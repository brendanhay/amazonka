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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetBuilds' smart constructor.
data BatchGetBuilds = BatchGetBuilds'
  { -- | The IDs of the builds.
    ids :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  BatchGetBuilds
newBatchGetBuilds pIds_ =
  BatchGetBuilds' {ids = Prelude._Coerce Lens.# pIds_}

-- | The IDs of the builds.
batchGetBuilds_ids :: Lens.Lens' BatchGetBuilds (Prelude.NonEmpty Prelude.Text)
batchGetBuilds_ids = Lens.lens (\BatchGetBuilds' {ids} -> ids) (\s@BatchGetBuilds' {} a -> s {ids = a} :: BatchGetBuilds) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest BatchGetBuilds where
  type Rs BatchGetBuilds = BatchGetBuildsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBuildsResponse'
            Prelude.<$> (x Prelude..?> "buildsNotFound")
            Prelude.<*> (x Prelude..?> "builds" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetBuilds

instance Prelude.NFData BatchGetBuilds

instance Prelude.ToHeaders BatchGetBuilds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.BatchGetBuilds" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON BatchGetBuilds where
  toJSON BatchGetBuilds' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ids" Prelude..= ids)]
      )

instance Prelude.ToPath BatchGetBuilds where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchGetBuilds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetBuildsResponse' smart constructor.
data BatchGetBuildsResponse = BatchGetBuildsResponse'
  { -- | The IDs of builds for which information could not be found.
    buildsNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Information about the requested builds.
    builds :: Prelude.Maybe [Build],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetBuildsResponse
newBatchGetBuildsResponse pHttpStatus_ =
  BatchGetBuildsResponse'
    { buildsNotFound =
        Prelude.Nothing,
      builds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of builds for which information could not be found.
batchGetBuildsResponse_buildsNotFound :: Lens.Lens' BatchGetBuildsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetBuildsResponse_buildsNotFound = Lens.lens (\BatchGetBuildsResponse' {buildsNotFound} -> buildsNotFound) (\s@BatchGetBuildsResponse' {} a -> s {buildsNotFound = a} :: BatchGetBuildsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the requested builds.
batchGetBuildsResponse_builds :: Lens.Lens' BatchGetBuildsResponse (Prelude.Maybe [Build])
batchGetBuildsResponse_builds = Lens.lens (\BatchGetBuildsResponse' {builds} -> builds) (\s@BatchGetBuildsResponse' {} a -> s {builds = a} :: BatchGetBuildsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchGetBuildsResponse_httpStatus :: Lens.Lens' BatchGetBuildsResponse Prelude.Int
batchGetBuildsResponse_httpStatus = Lens.lens (\BatchGetBuildsResponse' {httpStatus} -> httpStatus) (\s@BatchGetBuildsResponse' {} a -> s {httpStatus = a} :: BatchGetBuildsResponse)

instance Prelude.NFData BatchGetBuildsResponse
