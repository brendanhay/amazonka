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
-- Module      : Amazonka.Glue.GetDataflowGraph
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a Python script into a directed acyclic graph (DAG).
module Amazonka.Glue.GetDataflowGraph
  ( -- * Creating a Request
    GetDataflowGraph (..),
    newGetDataflowGraph,

    -- * Request Lenses
    getDataflowGraph_pythonScript,

    -- * Destructuring the Response
    GetDataflowGraphResponse (..),
    newGetDataflowGraphResponse,

    -- * Response Lenses
    getDataflowGraphResponse_dagEdges,
    getDataflowGraphResponse_dagNodes,
    getDataflowGraphResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataflowGraph' smart constructor.
data GetDataflowGraph = GetDataflowGraph'
  { -- | The Python script to transform.
    pythonScript :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  GetDataflowGraph' {pythonScript = Prelude.Nothing}

-- | The Python script to transform.
getDataflowGraph_pythonScript :: Lens.Lens' GetDataflowGraph (Prelude.Maybe Prelude.Text)
getDataflowGraph_pythonScript = Lens.lens (\GetDataflowGraph' {pythonScript} -> pythonScript) (\s@GetDataflowGraph' {} a -> s {pythonScript = a} :: GetDataflowGraph)

instance Core.AWSRequest GetDataflowGraph where
  type
    AWSResponse GetDataflowGraph =
      GetDataflowGraphResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataflowGraphResponse'
            Prelude.<$> (x Data..?> "DagEdges" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "DagNodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataflowGraph where
  hashWithSalt _salt GetDataflowGraph' {..} =
    _salt `Prelude.hashWithSalt` pythonScript

instance Prelude.NFData GetDataflowGraph where
  rnf GetDataflowGraph' {..} = Prelude.rnf pythonScript

instance Data.ToHeaders GetDataflowGraph where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetDataflowGraph" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataflowGraph where
  toJSON GetDataflowGraph' {..} =
    Data.object
      ( Prelude.catMaybes
          [("PythonScript" Data..=) Prelude.<$> pythonScript]
      )

instance Data.ToPath GetDataflowGraph where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDataflowGraph where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataflowGraphResponse' smart constructor.
data GetDataflowGraphResponse = GetDataflowGraphResponse'
  { -- | A list of the edges in the resulting DAG.
    dagEdges :: Prelude.Maybe [CodeGenEdge],
    -- | A list of the nodes in the resulting DAG.
    dagNodes :: Prelude.Maybe [CodeGenNode],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataflowGraphResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dagEdges', 'getDataflowGraphResponse_dagEdges' - A list of the edges in the resulting DAG.
--
-- 'dagNodes', 'getDataflowGraphResponse_dagNodes' - A list of the nodes in the resulting DAG.
--
-- 'httpStatus', 'getDataflowGraphResponse_httpStatus' - The response's http status code.
newGetDataflowGraphResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataflowGraphResponse
newGetDataflowGraphResponse pHttpStatus_ =
  GetDataflowGraphResponse'
    { dagEdges =
        Prelude.Nothing,
      dagNodes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the edges in the resulting DAG.
getDataflowGraphResponse_dagEdges :: Lens.Lens' GetDataflowGraphResponse (Prelude.Maybe [CodeGenEdge])
getDataflowGraphResponse_dagEdges = Lens.lens (\GetDataflowGraphResponse' {dagEdges} -> dagEdges) (\s@GetDataflowGraphResponse' {} a -> s {dagEdges = a} :: GetDataflowGraphResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the nodes in the resulting DAG.
getDataflowGraphResponse_dagNodes :: Lens.Lens' GetDataflowGraphResponse (Prelude.Maybe [CodeGenNode])
getDataflowGraphResponse_dagNodes = Lens.lens (\GetDataflowGraphResponse' {dagNodes} -> dagNodes) (\s@GetDataflowGraphResponse' {} a -> s {dagNodes = a} :: GetDataflowGraphResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDataflowGraphResponse_httpStatus :: Lens.Lens' GetDataflowGraphResponse Prelude.Int
getDataflowGraphResponse_httpStatus = Lens.lens (\GetDataflowGraphResponse' {httpStatus} -> httpStatus) (\s@GetDataflowGraphResponse' {} a -> s {httpStatus = a} :: GetDataflowGraphResponse)

instance Prelude.NFData GetDataflowGraphResponse where
  rnf GetDataflowGraphResponse' {..} =
    Prelude.rnf dagEdges
      `Prelude.seq` Prelude.rnf dagNodes
      `Prelude.seq` Prelude.rnf httpStatus
