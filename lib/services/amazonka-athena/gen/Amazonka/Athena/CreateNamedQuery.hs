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
-- Module      : Amazonka.Athena.CreateNamedQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a named query in the specified workgroup. Requires that you have
-- access to the workgroup.
--
-- For code samples using the Amazon Web Services SDK for Java, see
-- <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
module Amazonka.Athena.CreateNamedQuery
  ( -- * Creating a Request
    CreateNamedQuery (..),
    newCreateNamedQuery,

    -- * Request Lenses
    createNamedQuery_clientRequestToken,
    createNamedQuery_workGroup,
    createNamedQuery_description,
    createNamedQuery_name,
    createNamedQuery_database,
    createNamedQuery_queryString,

    -- * Destructuring the Response
    CreateNamedQueryResponse (..),
    newCreateNamedQueryResponse,

    -- * Response Lenses
    createNamedQueryResponse_namedQueryId,
    createNamedQueryResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNamedQuery' smart constructor.
data CreateNamedQuery = CreateNamedQuery'
  { -- | A unique case-sensitive string used to ensure the request to create the
    -- query is idempotent (executes only once). If another @CreateNamedQuery@
    -- request is received, the same response is returned and another query is
    -- not created. If a parameter has changed, for example, the @QueryString@,
    -- an error is returned.
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for users. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the workgroup in which the named query is being created.
    workGroup :: Prelude.Maybe Prelude.Text,
    -- | The query description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The query name.
    name :: Prelude.Text,
    -- | The database to which the query belongs.
    database :: Prelude.Text,
    -- | The contents of the query with all query statements.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNamedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createNamedQuery_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- query is idempotent (executes only once). If another @CreateNamedQuery@
-- request is received, the same response is returned and another query is
-- not created. If a parameter has changed, for example, the @QueryString@,
-- an error is returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'workGroup', 'createNamedQuery_workGroup' - The name of the workgroup in which the named query is being created.
--
-- 'description', 'createNamedQuery_description' - The query description.
--
-- 'name', 'createNamedQuery_name' - The query name.
--
-- 'database', 'createNamedQuery_database' - The database to which the query belongs.
--
-- 'queryString', 'createNamedQuery_queryString' - The contents of the query with all query statements.
newCreateNamedQuery ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  CreateNamedQuery
newCreateNamedQuery pName_ pDatabase_ pQueryString_ =
  CreateNamedQuery'
    { clientRequestToken =
        Prelude.Nothing,
      workGroup = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      queryString = pQueryString_
    }

-- | A unique case-sensitive string used to ensure the request to create the
-- query is idempotent (executes only once). If another @CreateNamedQuery@
-- request is received, the same response is returned and another query is
-- not created. If a parameter has changed, for example, the @QueryString@,
-- an error is returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
createNamedQuery_clientRequestToken :: Lens.Lens' CreateNamedQuery (Prelude.Maybe Prelude.Text)
createNamedQuery_clientRequestToken = Lens.lens (\CreateNamedQuery' {clientRequestToken} -> clientRequestToken) (\s@CreateNamedQuery' {} a -> s {clientRequestToken = a} :: CreateNamedQuery)

-- | The name of the workgroup in which the named query is being created.
createNamedQuery_workGroup :: Lens.Lens' CreateNamedQuery (Prelude.Maybe Prelude.Text)
createNamedQuery_workGroup = Lens.lens (\CreateNamedQuery' {workGroup} -> workGroup) (\s@CreateNamedQuery' {} a -> s {workGroup = a} :: CreateNamedQuery)

-- | The query description.
createNamedQuery_description :: Lens.Lens' CreateNamedQuery (Prelude.Maybe Prelude.Text)
createNamedQuery_description = Lens.lens (\CreateNamedQuery' {description} -> description) (\s@CreateNamedQuery' {} a -> s {description = a} :: CreateNamedQuery)

-- | The query name.
createNamedQuery_name :: Lens.Lens' CreateNamedQuery Prelude.Text
createNamedQuery_name = Lens.lens (\CreateNamedQuery' {name} -> name) (\s@CreateNamedQuery' {} a -> s {name = a} :: CreateNamedQuery)

-- | The database to which the query belongs.
createNamedQuery_database :: Lens.Lens' CreateNamedQuery Prelude.Text
createNamedQuery_database = Lens.lens (\CreateNamedQuery' {database} -> database) (\s@CreateNamedQuery' {} a -> s {database = a} :: CreateNamedQuery)

-- | The contents of the query with all query statements.
createNamedQuery_queryString :: Lens.Lens' CreateNamedQuery Prelude.Text
createNamedQuery_queryString = Lens.lens (\CreateNamedQuery' {queryString} -> queryString) (\s@CreateNamedQuery' {} a -> s {queryString = a} :: CreateNamedQuery)

instance Core.AWSRequest CreateNamedQuery where
  type
    AWSResponse CreateNamedQuery =
      CreateNamedQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNamedQueryResponse'
            Prelude.<$> (x Data..?> "NamedQueryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNamedQuery where
  hashWithSalt _salt CreateNamedQuery' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData CreateNamedQuery where
  rnf CreateNamedQuery' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders CreateNamedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.CreateNamedQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNamedQuery where
  toJSON CreateNamedQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("WorkGroup" Data..=) Prelude.<$> workGroup,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("QueryString" Data..= queryString)
          ]
      )

instance Data.ToPath CreateNamedQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNamedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNamedQueryResponse' smart constructor.
data CreateNamedQueryResponse = CreateNamedQueryResponse'
  { -- | The unique ID of the query.
    namedQueryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNamedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQueryId', 'createNamedQueryResponse_namedQueryId' - The unique ID of the query.
--
-- 'httpStatus', 'createNamedQueryResponse_httpStatus' - The response's http status code.
newCreateNamedQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNamedQueryResponse
newCreateNamedQueryResponse pHttpStatus_ =
  CreateNamedQueryResponse'
    { namedQueryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the query.
createNamedQueryResponse_namedQueryId :: Lens.Lens' CreateNamedQueryResponse (Prelude.Maybe Prelude.Text)
createNamedQueryResponse_namedQueryId = Lens.lens (\CreateNamedQueryResponse' {namedQueryId} -> namedQueryId) (\s@CreateNamedQueryResponse' {} a -> s {namedQueryId = a} :: CreateNamedQueryResponse)

-- | The response's http status code.
createNamedQueryResponse_httpStatus :: Lens.Lens' CreateNamedQueryResponse Prelude.Int
createNamedQueryResponse_httpStatus = Lens.lens (\CreateNamedQueryResponse' {httpStatus} -> httpStatus) (\s@CreateNamedQueryResponse' {} a -> s {httpStatus = a} :: CreateNamedQueryResponse)

instance Prelude.NFData CreateNamedQueryResponse where
  rnf CreateNamedQueryResponse' {..} =
    Prelude.rnf namedQueryId
      `Prelude.seq` Prelude.rnf httpStatus
