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
-- Module      : Amazonka.Athena.CreateNotebook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty @ipynb@ file in the specified Apache Spark enabled
-- workgroup. Throws an error if a file in the workgroup with the same name
-- already exists.
module Amazonka.Athena.CreateNotebook
  ( -- * Creating a Request
    CreateNotebook (..),
    newCreateNotebook,

    -- * Request Lenses
    createNotebook_clientRequestToken,
    createNotebook_workGroup,
    createNotebook_name,

    -- * Destructuring the Response
    CreateNotebookResponse (..),
    newCreateNotebookResponse,

    -- * Response Lenses
    createNotebookResponse_notebookId,
    createNotebookResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNotebook' smart constructor.
data CreateNotebook = CreateNotebook'
  { -- | A unique case-sensitive string used to ensure the request to create the
    -- notebook is idempotent (executes only once).
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for you. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Spark enabled workgroup in which the notebook will be
    -- created.
    workGroup :: Prelude.Text,
    -- | The name of the @ipynb@ file to be created in the Spark workgroup,
    -- without the @.ipynb@ extension.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotebook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createNotebook_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'workGroup', 'createNotebook_workGroup' - The name of the Spark enabled workgroup in which the notebook will be
-- created.
--
-- 'name', 'createNotebook_name' - The name of the @ipynb@ file to be created in the Spark workgroup,
-- without the @.ipynb@ extension.
newCreateNotebook ::
  -- | 'workGroup'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateNotebook
newCreateNotebook pWorkGroup_ pName_ =
  CreateNotebook'
    { clientRequestToken =
        Prelude.Nothing,
      workGroup = pWorkGroup_,
      name = pName_
    }

-- | A unique case-sensitive string used to ensure the request to create the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
createNotebook_clientRequestToken :: Lens.Lens' CreateNotebook (Prelude.Maybe Prelude.Text)
createNotebook_clientRequestToken = Lens.lens (\CreateNotebook' {clientRequestToken} -> clientRequestToken) (\s@CreateNotebook' {} a -> s {clientRequestToken = a} :: CreateNotebook)

-- | The name of the Spark enabled workgroup in which the notebook will be
-- created.
createNotebook_workGroup :: Lens.Lens' CreateNotebook Prelude.Text
createNotebook_workGroup = Lens.lens (\CreateNotebook' {workGroup} -> workGroup) (\s@CreateNotebook' {} a -> s {workGroup = a} :: CreateNotebook)

-- | The name of the @ipynb@ file to be created in the Spark workgroup,
-- without the @.ipynb@ extension.
createNotebook_name :: Lens.Lens' CreateNotebook Prelude.Text
createNotebook_name = Lens.lens (\CreateNotebook' {name} -> name) (\s@CreateNotebook' {} a -> s {name = a} :: CreateNotebook)

instance Core.AWSRequest CreateNotebook where
  type
    AWSResponse CreateNotebook =
      CreateNotebookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNotebookResponse'
            Prelude.<$> (x Data..?> "NotebookId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNotebook where
  hashWithSalt _salt CreateNotebook' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateNotebook where
  rnf CreateNotebook' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateNotebook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.CreateNotebook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNotebook where
  toJSON CreateNotebook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("WorkGroup" Data..= workGroup),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateNotebook where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNotebook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNotebookResponse' smart constructor.
data CreateNotebookResponse = CreateNotebookResponse'
  { -- | A unique identifier for the notebook.
    notebookId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotebookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookId', 'createNotebookResponse_notebookId' - A unique identifier for the notebook.
--
-- 'httpStatus', 'createNotebookResponse_httpStatus' - The response's http status code.
newCreateNotebookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNotebookResponse
newCreateNotebookResponse pHttpStatus_ =
  CreateNotebookResponse'
    { notebookId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the notebook.
createNotebookResponse_notebookId :: Lens.Lens' CreateNotebookResponse (Prelude.Maybe Prelude.Text)
createNotebookResponse_notebookId = Lens.lens (\CreateNotebookResponse' {notebookId} -> notebookId) (\s@CreateNotebookResponse' {} a -> s {notebookId = a} :: CreateNotebookResponse)

-- | The response's http status code.
createNotebookResponse_httpStatus :: Lens.Lens' CreateNotebookResponse Prelude.Int
createNotebookResponse_httpStatus = Lens.lens (\CreateNotebookResponse' {httpStatus} -> httpStatus) (\s@CreateNotebookResponse' {} a -> s {httpStatus = a} :: CreateNotebookResponse)

instance Prelude.NFData CreateNotebookResponse where
  rnf CreateNotebookResponse' {..} =
    Prelude.rnf notebookId
      `Prelude.seq` Prelude.rnf httpStatus
