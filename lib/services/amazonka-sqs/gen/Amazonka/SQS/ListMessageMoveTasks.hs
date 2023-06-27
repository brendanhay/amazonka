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
-- Module      : Amazonka.SQS.ListMessageMoveTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the most recent message movement tasks (up to 10) under a specific
-- source queue.
module Amazonka.SQS.ListMessageMoveTasks
  ( -- * Creating a Request
    ListMessageMoveTasks (..),
    newListMessageMoveTasks,

    -- * Request Lenses
    listMessageMoveTasks_maxResults,
    listMessageMoveTasks_sourceArn,

    -- * Destructuring the Response
    ListMessageMoveTasksResponse (..),
    newListMessageMoveTasksResponse,

    -- * Response Lenses
    listMessageMoveTasksResponse_results,
    listMessageMoveTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- | /See:/ 'newListMessageMoveTasks' smart constructor.
data ListMessageMoveTasks = ListMessageMoveTasks'
  { -- | The maximum number of results to include in the response. The default is
    -- 1, which provides the most recent message movement task. The upper limit
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the queue whose message movement tasks are to be listed.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMessageMoveTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMessageMoveTasks_maxResults' - The maximum number of results to include in the response. The default is
-- 1, which provides the most recent message movement task. The upper limit
-- is 10.
--
-- 'sourceArn', 'listMessageMoveTasks_sourceArn' - The ARN of the queue whose message movement tasks are to be listed.
newListMessageMoveTasks ::
  -- | 'sourceArn'
  Prelude.Text ->
  ListMessageMoveTasks
newListMessageMoveTasks pSourceArn_ =
  ListMessageMoveTasks'
    { maxResults = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The maximum number of results to include in the response. The default is
-- 1, which provides the most recent message movement task. The upper limit
-- is 10.
listMessageMoveTasks_maxResults :: Lens.Lens' ListMessageMoveTasks (Prelude.Maybe Prelude.Int)
listMessageMoveTasks_maxResults = Lens.lens (\ListMessageMoveTasks' {maxResults} -> maxResults) (\s@ListMessageMoveTasks' {} a -> s {maxResults = a} :: ListMessageMoveTasks)

-- | The ARN of the queue whose message movement tasks are to be listed.
listMessageMoveTasks_sourceArn :: Lens.Lens' ListMessageMoveTasks Prelude.Text
listMessageMoveTasks_sourceArn = Lens.lens (\ListMessageMoveTasks' {sourceArn} -> sourceArn) (\s@ListMessageMoveTasks' {} a -> s {sourceArn = a} :: ListMessageMoveTasks)

instance Core.AWSRequest ListMessageMoveTasks where
  type
    AWSResponse ListMessageMoveTasks =
      ListMessageMoveTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListMessageMoveTasksResult"
      ( \s h x ->
          ListMessageMoveTasksResponse'
            Prelude.<$> ( Core.may
                            (Data.parseXMLList "ListMessageMoveTasksResultEntry")
                            x
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMessageMoveTasks where
  hashWithSalt _salt ListMessageMoveTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData ListMessageMoveTasks where
  rnf ListMessageMoveTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf sourceArn

instance Data.ToHeaders ListMessageMoveTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMessageMoveTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMessageMoveTasks where
  toQuery ListMessageMoveTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListMessageMoveTasks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "MaxResults" Data.=: maxResults,
        "SourceArn" Data.=: sourceArn
      ]

-- | /See:/ 'newListMessageMoveTasksResponse' smart constructor.
data ListMessageMoveTasksResponse = ListMessageMoveTasksResponse'
  { -- | A list of message movement tasks and their attributes.
    results :: Prelude.Maybe [ListMessageMoveTasksResultEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMessageMoveTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'results', 'listMessageMoveTasksResponse_results' - A list of message movement tasks and their attributes.
--
-- 'httpStatus', 'listMessageMoveTasksResponse_httpStatus' - The response's http status code.
newListMessageMoveTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMessageMoveTasksResponse
newListMessageMoveTasksResponse pHttpStatus_ =
  ListMessageMoveTasksResponse'
    { results =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of message movement tasks and their attributes.
listMessageMoveTasksResponse_results :: Lens.Lens' ListMessageMoveTasksResponse (Prelude.Maybe [ListMessageMoveTasksResultEntry])
listMessageMoveTasksResponse_results = Lens.lens (\ListMessageMoveTasksResponse' {results} -> results) (\s@ListMessageMoveTasksResponse' {} a -> s {results = a} :: ListMessageMoveTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMessageMoveTasksResponse_httpStatus :: Lens.Lens' ListMessageMoveTasksResponse Prelude.Int
listMessageMoveTasksResponse_httpStatus = Lens.lens (\ListMessageMoveTasksResponse' {httpStatus} -> httpStatus) (\s@ListMessageMoveTasksResponse' {} a -> s {httpStatus = a} :: ListMessageMoveTasksResponse)

instance Prelude.NFData ListMessageMoveTasksResponse where
  rnf ListMessageMoveTasksResponse' {..} =
    Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus
