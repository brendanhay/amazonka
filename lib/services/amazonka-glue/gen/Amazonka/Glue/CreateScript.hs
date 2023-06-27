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
-- Module      : Amazonka.Glue.CreateScript
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a directed acyclic graph (DAG) into code.
module Amazonka.Glue.CreateScript
  ( -- * Creating a Request
    CreateScript (..),
    newCreateScript,

    -- * Request Lenses
    createScript_dagEdges,
    createScript_dagNodes,
    createScript_language,

    -- * Destructuring the Response
    CreateScriptResponse (..),
    newCreateScriptResponse,

    -- * Response Lenses
    createScriptResponse_pythonScript,
    createScriptResponse_scalaCode,
    createScriptResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateScript' smart constructor.
data CreateScript = CreateScript'
  { -- | A list of the edges in the DAG.
    dagEdges :: Prelude.Maybe [CodeGenEdge],
    -- | A list of the nodes in the DAG.
    dagNodes :: Prelude.Maybe [CodeGenNode],
    -- | The programming language of the resulting code from the DAG.
    language :: Prelude.Maybe Language
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dagEdges', 'createScript_dagEdges' - A list of the edges in the DAG.
--
-- 'dagNodes', 'createScript_dagNodes' - A list of the nodes in the DAG.
--
-- 'language', 'createScript_language' - The programming language of the resulting code from the DAG.
newCreateScript ::
  CreateScript
newCreateScript =
  CreateScript'
    { dagEdges = Prelude.Nothing,
      dagNodes = Prelude.Nothing,
      language = Prelude.Nothing
    }

-- | A list of the edges in the DAG.
createScript_dagEdges :: Lens.Lens' CreateScript (Prelude.Maybe [CodeGenEdge])
createScript_dagEdges = Lens.lens (\CreateScript' {dagEdges} -> dagEdges) (\s@CreateScript' {} a -> s {dagEdges = a} :: CreateScript) Prelude.. Lens.mapping Lens.coerced

-- | A list of the nodes in the DAG.
createScript_dagNodes :: Lens.Lens' CreateScript (Prelude.Maybe [CodeGenNode])
createScript_dagNodes = Lens.lens (\CreateScript' {dagNodes} -> dagNodes) (\s@CreateScript' {} a -> s {dagNodes = a} :: CreateScript) Prelude.. Lens.mapping Lens.coerced

-- | The programming language of the resulting code from the DAG.
createScript_language :: Lens.Lens' CreateScript (Prelude.Maybe Language)
createScript_language = Lens.lens (\CreateScript' {language} -> language) (\s@CreateScript' {} a -> s {language = a} :: CreateScript)

instance Core.AWSRequest CreateScript where
  type AWSResponse CreateScript = CreateScriptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScriptResponse'
            Prelude.<$> (x Data..?> "PythonScript")
            Prelude.<*> (x Data..?> "ScalaCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateScript where
  hashWithSalt _salt CreateScript' {..} =
    _salt
      `Prelude.hashWithSalt` dagEdges
      `Prelude.hashWithSalt` dagNodes
      `Prelude.hashWithSalt` language

instance Prelude.NFData CreateScript where
  rnf CreateScript' {..} =
    Prelude.rnf dagEdges
      `Prelude.seq` Prelude.rnf dagNodes
      `Prelude.seq` Prelude.rnf language

instance Data.ToHeaders CreateScript where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateScript" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateScript where
  toJSON CreateScript' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DagEdges" Data..=) Prelude.<$> dagEdges,
            ("DagNodes" Data..=) Prelude.<$> dagNodes,
            ("Language" Data..=) Prelude.<$> language
          ]
      )

instance Data.ToPath CreateScript where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateScript where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateScriptResponse where
  rnf CreateScriptResponse' {..} =
    Prelude.rnf pythonScript
      `Prelude.seq` Prelude.rnf scalaCode
      `Prelude.seq` Prelude.rnf httpStatus
