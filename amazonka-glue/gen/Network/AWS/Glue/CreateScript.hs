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
-- Module      : Network.AWS.Glue.CreateScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a directed acyclic graph (DAG) into code.
module Network.AWS.Glue.CreateScript
  ( -- * Creating a Request
    CreateScript (..),
    newCreateScript,

    -- * Request Lenses
    createScript_dagNodes,
    createScript_language,
    createScript_dagEdges,

    -- * Destructuring the Response
    CreateScriptResponse (..),
    newCreateScriptResponse,

    -- * Response Lenses
    createScriptResponse_pythonScript,
    createScriptResponse_scalaCode,
    createScriptResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateScript' smart constructor.
data CreateScript = CreateScript'
  { -- | A list of the nodes in the DAG.
    dagNodes :: Prelude.Maybe [CodeGenNode],
    -- | The programming language of the resulting code from the DAG.
    language :: Prelude.Maybe Language,
    -- | A list of the edges in the DAG.
    dagEdges :: Prelude.Maybe [CodeGenEdge]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dagNodes', 'createScript_dagNodes' - A list of the nodes in the DAG.
--
-- 'language', 'createScript_language' - The programming language of the resulting code from the DAG.
--
-- 'dagEdges', 'createScript_dagEdges' - A list of the edges in the DAG.
newCreateScript ::
  CreateScript
newCreateScript =
  CreateScript'
    { dagNodes = Prelude.Nothing,
      language = Prelude.Nothing,
      dagEdges = Prelude.Nothing
    }

-- | A list of the nodes in the DAG.
createScript_dagNodes :: Lens.Lens' CreateScript (Prelude.Maybe [CodeGenNode])
createScript_dagNodes = Lens.lens (\CreateScript' {dagNodes} -> dagNodes) (\s@CreateScript' {} a -> s {dagNodes = a} :: CreateScript) Prelude.. Lens.mapping Prelude._Coerce

-- | The programming language of the resulting code from the DAG.
createScript_language :: Lens.Lens' CreateScript (Prelude.Maybe Language)
createScript_language = Lens.lens (\CreateScript' {language} -> language) (\s@CreateScript' {} a -> s {language = a} :: CreateScript)

-- | A list of the edges in the DAG.
createScript_dagEdges :: Lens.Lens' CreateScript (Prelude.Maybe [CodeGenEdge])
createScript_dagEdges = Lens.lens (\CreateScript' {dagEdges} -> dagEdges) (\s@CreateScript' {} a -> s {dagEdges = a} :: CreateScript) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest CreateScript where
  type Rs CreateScript = CreateScriptResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScriptResponse'
            Prelude.<$> (x Prelude..?> "PythonScript")
            Prelude.<*> (x Prelude..?> "ScalaCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateScript

instance Prelude.NFData CreateScript

instance Prelude.ToHeaders CreateScript where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.CreateScript" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateScript where
  toJSON CreateScript' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DagNodes" Prelude..=) Prelude.<$> dagNodes,
            ("Language" Prelude..=) Prelude.<$> language,
            ("DagEdges" Prelude..=) Prelude.<$> dagEdges
          ]
      )

instance Prelude.ToPath CreateScript where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateScript where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { -- | The Python script generated from the DAG.
    pythonScript :: Prelude.Maybe Prelude.Text,
    -- | The Scala code generated from the DAG.
    scalaCode :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateScriptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pythonScript', 'createScriptResponse_pythonScript' - The Python script generated from the DAG.
--
-- 'scalaCode', 'createScriptResponse_scalaCode' - The Scala code generated from the DAG.
--
-- 'httpStatus', 'createScriptResponse_httpStatus' - The response's http status code.
newCreateScriptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateScriptResponse
newCreateScriptResponse pHttpStatus_ =
  CreateScriptResponse'
    { pythonScript =
        Prelude.Nothing,
      scalaCode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Python script generated from the DAG.
createScriptResponse_pythonScript :: Lens.Lens' CreateScriptResponse (Prelude.Maybe Prelude.Text)
createScriptResponse_pythonScript = Lens.lens (\CreateScriptResponse' {pythonScript} -> pythonScript) (\s@CreateScriptResponse' {} a -> s {pythonScript = a} :: CreateScriptResponse)

-- | The Scala code generated from the DAG.
createScriptResponse_scalaCode :: Lens.Lens' CreateScriptResponse (Prelude.Maybe Prelude.Text)
createScriptResponse_scalaCode = Lens.lens (\CreateScriptResponse' {scalaCode} -> scalaCode) (\s@CreateScriptResponse' {} a -> s {scalaCode = a} :: CreateScriptResponse)

-- | The response's http status code.
createScriptResponse_httpStatus :: Lens.Lens' CreateScriptResponse Prelude.Int
createScriptResponse_httpStatus = Lens.lens (\CreateScriptResponse' {httpStatus} -> httpStatus) (\s@CreateScriptResponse' {} a -> s {httpStatus = a} :: CreateScriptResponse)

instance Prelude.NFData CreateScriptResponse
