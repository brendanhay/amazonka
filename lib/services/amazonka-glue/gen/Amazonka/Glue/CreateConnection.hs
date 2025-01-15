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
-- Module      : Amazonka.Glue.CreateConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection definition in the Data Catalog.
module Amazonka.Glue.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_catalogId,
    createConnection_tags,
    createConnection_connectionInput,

    -- * Destructuring the Response
    CreateConnectionResponse (..),
    newCreateConnectionResponse,

    -- * Response Lenses
    createConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The ID of the Data Catalog in which to create the connection. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The tags you assign to the connection.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A @ConnectionInput@ object defining the connection to create.
    connectionInput :: ConnectionInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createConnection_catalogId' - The ID of the Data Catalog in which to create the connection. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'tags', 'createConnection_tags' - The tags you assign to the connection.
--
-- 'connectionInput', 'createConnection_connectionInput' - A @ConnectionInput@ object defining the connection to create.
newCreateConnection ::
  -- | 'connectionInput'
  ConnectionInput ->
  CreateConnection
newCreateConnection pConnectionInput_ =
  CreateConnection'
    { catalogId = Prelude.Nothing,
      tags = Prelude.Nothing,
      connectionInput = pConnectionInput_
    }

-- | The ID of the Data Catalog in which to create the connection. If none is
-- provided, the Amazon Web Services account ID is used by default.
createConnection_catalogId :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_catalogId = Lens.lens (\CreateConnection' {catalogId} -> catalogId) (\s@CreateConnection' {} a -> s {catalogId = a} :: CreateConnection)

-- | The tags you assign to the connection.
createConnection_tags :: Lens.Lens' CreateConnection (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConnection_tags = Lens.lens (\CreateConnection' {tags} -> tags) (\s@CreateConnection' {} a -> s {tags = a} :: CreateConnection) Prelude.. Lens.mapping Lens.coerced

-- | A @ConnectionInput@ object defining the connection to create.
createConnection_connectionInput :: Lens.Lens' CreateConnection ConnectionInput
createConnection_connectionInput = Lens.lens (\CreateConnection' {connectionInput} -> connectionInput) (\s@CreateConnection' {} a -> s {connectionInput = a} :: CreateConnection)

instance Core.AWSRequest CreateConnection where
  type
    AWSResponse CreateConnection =
      CreateConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnection where
  hashWithSalt _salt CreateConnection' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` connectionInput

instance Prelude.NFData CreateConnection where
  rnf CreateConnection' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf connectionInput

instance Data.ToHeaders CreateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateConnection" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ConnectionInput" Data..= connectionInput)
          ]
      )

instance Data.ToPath CreateConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectionResponse' smart constructor.
data CreateConnectionResponse = CreateConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConnectionResponse_httpStatus' - The response's http status code.
newCreateConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectionResponse
newCreateConnectionResponse pHttpStatus_ =
  CreateConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createConnectionResponse_httpStatus :: Lens.Lens' CreateConnectionResponse Prelude.Int
createConnectionResponse_httpStatus = Lens.lens (\CreateConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionResponse' {} a -> s {httpStatus = a} :: CreateConnectionResponse)

instance Prelude.NFData CreateConnectionResponse where
  rnf CreateConnectionResponse' {..} =
    Prelude.rnf httpStatus
