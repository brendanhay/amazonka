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
-- Module      : Amazonka.AppSync.StartSchemaCreation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new schema to your GraphQL API.
--
-- This operation is asynchronous. Use to determine when it has completed.
module Amazonka.AppSync.StartSchemaCreation
  ( -- * Creating a Request
    StartSchemaCreation (..),
    newStartSchemaCreation,

    -- * Request Lenses
    startSchemaCreation_apiId,
    startSchemaCreation_definition,

    -- * Destructuring the Response
    StartSchemaCreationResponse (..),
    newStartSchemaCreationResponse,

    -- * Response Lenses
    startSchemaCreationResponse_status,
    startSchemaCreationResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSchemaCreation' smart constructor.
data StartSchemaCreation = StartSchemaCreation'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The schema definition, in GraphQL schema language format.
    definition :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSchemaCreation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'startSchemaCreation_apiId' - The API ID.
--
-- 'definition', 'startSchemaCreation_definition' - The schema definition, in GraphQL schema language format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newStartSchemaCreation ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'definition'
  Prelude.ByteString ->
  StartSchemaCreation
newStartSchemaCreation pApiId_ pDefinition_ =
  StartSchemaCreation'
    { apiId = pApiId_,
      definition = Data._Base64 Lens.# pDefinition_
    }

-- | The API ID.
startSchemaCreation_apiId :: Lens.Lens' StartSchemaCreation Prelude.Text
startSchemaCreation_apiId = Lens.lens (\StartSchemaCreation' {apiId} -> apiId) (\s@StartSchemaCreation' {} a -> s {apiId = a} :: StartSchemaCreation)

-- | The schema definition, in GraphQL schema language format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
startSchemaCreation_definition :: Lens.Lens' StartSchemaCreation Prelude.ByteString
startSchemaCreation_definition = Lens.lens (\StartSchemaCreation' {definition} -> definition) (\s@StartSchemaCreation' {} a -> s {definition = a} :: StartSchemaCreation) Prelude.. Data._Base64

instance Core.AWSRequest StartSchemaCreation where
  type
    AWSResponse StartSchemaCreation =
      StartSchemaCreationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSchemaCreationResponse'
            Prelude.<$> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSchemaCreation where
  hashWithSalt _salt StartSchemaCreation' {..} =
    _salt
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` definition

instance Prelude.NFData StartSchemaCreation where
  rnf StartSchemaCreation' {..} =
    Prelude.rnf apiId `Prelude.seq`
      Prelude.rnf definition

instance Data.ToHeaders StartSchemaCreation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSchemaCreation where
  toJSON StartSchemaCreation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("definition" Data..= definition)]
      )

instance Data.ToPath StartSchemaCreation where
  toPath StartSchemaCreation' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/schemacreation"]

instance Data.ToQuery StartSchemaCreation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSchemaCreationResponse' smart constructor.
data StartSchemaCreationResponse = StartSchemaCreationResponse'
  { -- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or
    -- NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add
    -- data.
    status :: Prelude.Maybe SchemaStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSchemaCreationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'startSchemaCreationResponse_status' - The current state of the schema (PROCESSING, FAILED, SUCCESS, or
-- NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add
-- data.
--
-- 'httpStatus', 'startSchemaCreationResponse_httpStatus' - The response's http status code.
newStartSchemaCreationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSchemaCreationResponse
newStartSchemaCreationResponse pHttpStatus_ =
  StartSchemaCreationResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or
-- NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add
-- data.
startSchemaCreationResponse_status :: Lens.Lens' StartSchemaCreationResponse (Prelude.Maybe SchemaStatus)
startSchemaCreationResponse_status = Lens.lens (\StartSchemaCreationResponse' {status} -> status) (\s@StartSchemaCreationResponse' {} a -> s {status = a} :: StartSchemaCreationResponse)

-- | The response's http status code.
startSchemaCreationResponse_httpStatus :: Lens.Lens' StartSchemaCreationResponse Prelude.Int
startSchemaCreationResponse_httpStatus = Lens.lens (\StartSchemaCreationResponse' {httpStatus} -> httpStatus) (\s@StartSchemaCreationResponse' {} a -> s {httpStatus = a} :: StartSchemaCreationResponse)

instance Prelude.NFData StartSchemaCreationResponse where
  rnf StartSchemaCreationResponse' {..} =
    Prelude.rnf status `Prelude.seq`
      Prelude.rnf httpStatus
