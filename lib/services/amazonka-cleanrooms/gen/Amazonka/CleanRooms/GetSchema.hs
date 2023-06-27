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
-- Module      : Amazonka.CleanRooms.GetSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the schema for a relation within a collaboration.
module Amazonka.CleanRooms.GetSchema
  ( -- * Creating a Request
    GetSchema (..),
    newGetSchema,

    -- * Request Lenses
    getSchema_collaborationIdentifier,
    getSchema_name,

    -- * Destructuring the Response
    GetSchemaResponse (..),
    newGetSchemaResponse,

    -- * Response Lenses
    getSchemaResponse_httpStatus,
    getSchemaResponse_schema,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSchema' smart constructor.
data GetSchema = GetSchema'
  { -- | A unique identifier for the collaboration that the schema belongs to.
    -- Currently accepts a collaboration ID.
    collaborationIdentifier :: Prelude.Text,
    -- | The name of the relation to retrieve the schema for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collaborationIdentifier', 'getSchema_collaborationIdentifier' - A unique identifier for the collaboration that the schema belongs to.
-- Currently accepts a collaboration ID.
--
-- 'name', 'getSchema_name' - The name of the relation to retrieve the schema for.
newGetSchema ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetSchema
newGetSchema pCollaborationIdentifier_ pName_ =
  GetSchema'
    { collaborationIdentifier =
        pCollaborationIdentifier_,
      name = pName_
    }

-- | A unique identifier for the collaboration that the schema belongs to.
-- Currently accepts a collaboration ID.
getSchema_collaborationIdentifier :: Lens.Lens' GetSchema Prelude.Text
getSchema_collaborationIdentifier = Lens.lens (\GetSchema' {collaborationIdentifier} -> collaborationIdentifier) (\s@GetSchema' {} a -> s {collaborationIdentifier = a} :: GetSchema)

-- | The name of the relation to retrieve the schema for.
getSchema_name :: Lens.Lens' GetSchema Prelude.Text
getSchema_name = Lens.lens (\GetSchema' {name} -> name) (\s@GetSchema' {} a -> s {name = a} :: GetSchema)

instance Core.AWSRequest GetSchema where
  type AWSResponse GetSchema = GetSchemaResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "schema")
      )

instance Prelude.Hashable GetSchema where
  hashWithSalt _salt GetSchema' {..} =
    _salt
      `Prelude.hashWithSalt` collaborationIdentifier
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetSchema where
  rnf GetSchema' {..} =
    Prelude.rnf collaborationIdentifier
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSchema where
  toPath GetSchema' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier,
        "/schemas/",
        Data.toBS name
      ]

instance Data.ToQuery GetSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaResponse' smart constructor.
data GetSchemaResponse = GetSchemaResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire schema object.
    schema :: Schema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSchemaResponse_httpStatus' - The response's http status code.
--
-- 'schema', 'getSchemaResponse_schema' - The entire schema object.
newGetSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'schema'
  Schema ->
  GetSchemaResponse
newGetSchemaResponse pHttpStatus_ pSchema_ =
  GetSchemaResponse'
    { httpStatus = pHttpStatus_,
      schema = pSchema_
    }

-- | The response's http status code.
getSchemaResponse_httpStatus :: Lens.Lens' GetSchemaResponse Prelude.Int
getSchemaResponse_httpStatus = Lens.lens (\GetSchemaResponse' {httpStatus} -> httpStatus) (\s@GetSchemaResponse' {} a -> s {httpStatus = a} :: GetSchemaResponse)

-- | The entire schema object.
getSchemaResponse_schema :: Lens.Lens' GetSchemaResponse Schema
getSchemaResponse_schema = Lens.lens (\GetSchemaResponse' {schema} -> schema) (\s@GetSchemaResponse' {} a -> s {schema = a} :: GetSchemaResponse)

instance Prelude.NFData GetSchemaResponse where
  rnf GetSchemaResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf schema
