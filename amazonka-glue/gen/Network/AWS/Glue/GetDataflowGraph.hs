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
-- Module      : Network.AWS.Glue.GetDataflowGraph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a Python script into a directed acyclic graph (DAG).
module Network.AWS.Glue.GetDataflowGraph
  ( -- * Creating a Request
    GetDataflowGraph (..),
    newGetDataflowGraph,

    -- * Request Lenses
    getDataflowGraph_pythonScript,

    -- * Destructuring the Response
    GetDataflowGraphResponse (..),
    newGetDataflowGraphResponse,

    -- * Response Lenses
    getDataflowGraphResponse_dagNodes,
    getDataflowGraphResponse_dagEdges,
    getDataflowGraphResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDataflowGraph' smart constructor.
data GetDataflowGraph = GetDataflowGraph'
  { -- | The Python script to transform.
    pythonScript :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDataflowGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pythonScript', 'getDataflowGraph_pythonScript' - The Python script to transform.
newGetDataflowGraph ::
  GetDataflowGraph
newGetDataflowGraph =
  GetDataflowGraph' {pythonScript = Core.Nothing}

-- | The Python script to transform.
getDataflowGraph_pythonScript :: Lens.Lens' GetDataflowGraph (Core.Maybe Core.Text)
getDataflowGraph_pythonScript = Lens.lens (\GetDataflowGraph' {pythonScript} -> pythonScript) (\s@GetDataflowGraph' {} a -> s {pythonScript = a} :: GetDataflowGraph)

instance Core.AWSRequest GetDataflowGraph where
  type
    AWSResponse GetDataflowGraph =
      GetDataflowGraphResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataflowGraphResponse'
            Core.<$> (x Core..?> "DagNodes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "DagEdges" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDataflowGraph

instance Core.NFData GetDataflowGraph

instance Core.ToHeaders GetDataflowGraph where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetDataflowGraph" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDataflowGraph where
  toJSON GetDataflowGraph' {..} =
    Core.object
      ( Core.catMaybes
          [("PythonScript" Core..=) Core.<$> pythonScript]
      )

instance Core.ToPath GetDataflowGraph where
  toPath = Core.const "/"

instance Core.ToQuery GetDataflowGraph where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDataflowGraphResponse' smart constructor.
data GetDataflowGraphResponse = GetDataflowGraphResponse'
  { -- | A list of the nodes in the resulting DAG.
    dagNodes :: Core.Maybe [CodeGenNode],
    -- | A list of the edges in the resulting DAG.
    dagEdges :: Core.Maybe [CodeGenEdge],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDataflowGraphResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dagNodes', 'getDataflowGraphResponse_dagNodes' - A list of the nodes in the resulting DAG.
--
-- 'dagEdges', 'getDataflowGraphResponse_dagEdges' - A list of the edges in the resulting DAG.
--
-- 'httpStatus', 'getDataflowGraphResponse_httpStatus' - The response's http status code.
newGetDataflowGraphResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDataflowGraphResponse
newGetDataflowGraphResponse pHttpStatus_ =
  GetDataflowGraphResponse'
    { dagNodes = Core.Nothing,
      dagEdges = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the nodes in the resulting DAG.
getDataflowGraphResponse_dagNodes :: Lens.Lens' GetDataflowGraphResponse (Core.Maybe [CodeGenNode])
getDataflowGraphResponse_dagNodes = Lens.lens (\GetDataflowGraphResponse' {dagNodes} -> dagNodes) (\s@GetDataflowGraphResponse' {} a -> s {dagNodes = a} :: GetDataflowGraphResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of the edges in the resulting DAG.
getDataflowGraphResponse_dagEdges :: Lens.Lens' GetDataflowGraphResponse (Core.Maybe [CodeGenEdge])
getDataflowGraphResponse_dagEdges = Lens.lens (\GetDataflowGraphResponse' {dagEdges} -> dagEdges) (\s@GetDataflowGraphResponse' {} a -> s {dagEdges = a} :: GetDataflowGraphResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDataflowGraphResponse_httpStatus :: Lens.Lens' GetDataflowGraphResponse Core.Int
getDataflowGraphResponse_httpStatus = Lens.lens (\GetDataflowGraphResponse' {httpStatus} -> httpStatus) (\s@GetDataflowGraphResponse' {} a -> s {httpStatus = a} :: GetDataflowGraphResponse)

instance Core.NFData GetDataflowGraphResponse
