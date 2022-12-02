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
-- Module      : Amazonka.CodeBuild.BatchGetBuilds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more builds.
module Amazonka.CodeBuild.BatchGetBuilds
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

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetBuilds' smart constructor.
data BatchGetBuilds = BatchGetBuilds'
  { -- | The IDs of the builds.
    ids :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  BatchGetBuilds' {ids = Lens.coerced Lens.# pIds_}

-- | The IDs of the builds.
batchGetBuilds_ids :: Lens.Lens' BatchGetBuilds (Prelude.NonEmpty Prelude.Text)
batchGetBuilds_ids = Lens.lens (\BatchGetBuilds' {ids} -> ids) (\s@BatchGetBuilds' {} a -> s {ids = a} :: BatchGetBuilds) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetBuilds where
  type
    AWSResponse BatchGetBuilds =
      BatchGetBuildsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBuildsResponse'
            Prelude.<$> (x Data..?> "buildsNotFound")
            Prelude.<*> (x Data..?> "builds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetBuilds where
  hashWithSalt _salt BatchGetBuilds' {..} =
    _salt `Prelude.hashWithSalt` ids

instance Prelude.NFData BatchGetBuilds where
  rnf BatchGetBuilds' {..} = Prelude.rnf ids

instance Data.ToHeaders BatchGetBuilds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.BatchGetBuilds" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetBuilds where
  toJSON BatchGetBuilds' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ids" Data..= ids)]
      )

instance Data.ToPath BatchGetBuilds where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetBuilds where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
batchGetBuildsResponse_buildsNotFound = Lens.lens (\BatchGetBuildsResponse' {buildsNotFound} -> buildsNotFound) (\s@BatchGetBuildsResponse' {} a -> s {buildsNotFound = a} :: BatchGetBuildsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the requested builds.
batchGetBuildsResponse_builds :: Lens.Lens' BatchGetBuildsResponse (Prelude.Maybe [Build])
batchGetBuildsResponse_builds = Lens.lens (\BatchGetBuildsResponse' {builds} -> builds) (\s@BatchGetBuildsResponse' {} a -> s {builds = a} :: BatchGetBuildsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetBuildsResponse_httpStatus :: Lens.Lens' BatchGetBuildsResponse Prelude.Int
batchGetBuildsResponse_httpStatus = Lens.lens (\BatchGetBuildsResponse' {httpStatus} -> httpStatus) (\s@BatchGetBuildsResponse' {} a -> s {httpStatus = a} :: BatchGetBuildsResponse)

instance Prelude.NFData BatchGetBuildsResponse where
  rnf BatchGetBuildsResponse' {..} =
    Prelude.rnf buildsNotFound
      `Prelude.seq` Prelude.rnf builds
      `Prelude.seq` Prelude.rnf httpStatus
