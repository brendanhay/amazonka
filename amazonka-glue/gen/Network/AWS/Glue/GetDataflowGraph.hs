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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDataflowGraph' smart constructor.
data GetDataflowGraph = GetDataflowGraph'
  { -- | The Python script to transform.
    pythonScript :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetDataflowGraph where
  type Rs GetDataflowGraph = GetDataflowGraphResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataflowGraphResponse'
            Prelude.<$> (x Prelude..?> "DagNodes" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "DagEdges" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataflowGraph

instance Prelude.NFData GetDataflowGraph

instance Prelude.ToHeaders GetDataflowGraph where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetDataflowGraph" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetDataflowGraph where
  toJSON GetDataflowGraph' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PythonScript" Prelude..=)
              Prelude.<$> pythonScript
          ]
      )

instance Prelude.ToPath GetDataflowGraph where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetDataflowGraph where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataflowGraphResponse' smart constructor.
data GetDataflowGraphResponse = GetDataflowGraphResponse'
  { -- | A list of the nodes in the resulting DAG.
    dagNodes :: Prelude.Maybe [CodeGenNode],
    -- | A list of the edges in the resulting DAG.
    dagEdges :: Prelude.Maybe [CodeGenEdge],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetDataflowGraphResponse
newGetDataflowGraphResponse pHttpStatus_ =
  GetDataflowGraphResponse'
    { dagNodes =
        Prelude.Nothing,
      dagEdges = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the nodes in the resulting DAG.
getDataflowGraphResponse_dagNodes :: Lens.Lens' GetDataflowGraphResponse (Prelude.Maybe [CodeGenNode])
getDataflowGraphResponse_dagNodes = Lens.lens (\GetDataflowGraphResponse' {dagNodes} -> dagNodes) (\s@GetDataflowGraphResponse' {} a -> s {dagNodes = a} :: GetDataflowGraphResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of the edges in the resulting DAG.
getDataflowGraphResponse_dagEdges :: Lens.Lens' GetDataflowGraphResponse (Prelude.Maybe [CodeGenEdge])
getDataflowGraphResponse_dagEdges = Lens.lens (\GetDataflowGraphResponse' {dagEdges} -> dagEdges) (\s@GetDataflowGraphResponse' {} a -> s {dagEdges = a} :: GetDataflowGraphResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getDataflowGraphResponse_httpStatus :: Lens.Lens' GetDataflowGraphResponse Prelude.Int
getDataflowGraphResponse_httpStatus = Lens.lens (\GetDataflowGraphResponse' {httpStatus} -> httpStatus) (\s@GetDataflowGraphResponse' {} a -> s {httpStatus = a} :: GetDataflowGraphResponse)

instance Prelude.NFData GetDataflowGraphResponse
