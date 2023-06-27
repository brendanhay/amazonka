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
-- Module      : Amazonka.Schemas.DeleteSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a schema definition.
module Amazonka.Schemas.DeleteSchema
  ( -- * Creating a Request
    DeleteSchema (..),
    newDeleteSchema,

    -- * Request Lenses
    deleteSchema_registryName,
    deleteSchema_schemaName,

    -- * Destructuring the Response
    DeleteSchemaResponse (..),
    newDeleteSchemaResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newDeleteSchema' smart constructor.
data DeleteSchema = DeleteSchema'
  { -- | The name of the registry.
    registryName :: Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'deleteSchema_registryName' - The name of the registry.
--
-- 'schemaName', 'deleteSchema_schemaName' - The name of the schema.
newDeleteSchema ::
  -- | 'registryName'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  DeleteSchema
newDeleteSchema pRegistryName_ pSchemaName_ =
  DeleteSchema'
    { registryName = pRegistryName_,
      schemaName = pSchemaName_
    }

-- | The name of the registry.
deleteSchema_registryName :: Lens.Lens' DeleteSchema Prelude.Text
deleteSchema_registryName = Lens.lens (\DeleteSchema' {registryName} -> registryName) (\s@DeleteSchema' {} a -> s {registryName = a} :: DeleteSchema)

-- | The name of the schema.
deleteSchema_schemaName :: Lens.Lens' DeleteSchema Prelude.Text
deleteSchema_schemaName = Lens.lens (\DeleteSchema' {schemaName} -> schemaName) (\s@DeleteSchema' {} a -> s {schemaName = a} :: DeleteSchema)

instance Core.AWSRequest DeleteSchema where
  type AWSResponse DeleteSchema = DeleteSchemaResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteSchemaResponse'

instance Prelude.Hashable DeleteSchema where
  hashWithSalt _salt DeleteSchema' {..} =
    _salt
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaName

instance Prelude.NFData DeleteSchema where
  rnf DeleteSchema' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName

instance Data.ToHeaders DeleteSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSchema where
  toPath DeleteSchema' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Data.toBS registryName,
        "/schemas/name/",
        Data.toBS schemaName
      ]

instance Data.ToQuery DeleteSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSchemaResponse ::
  DeleteSchemaResponse
newDeleteSchemaResponse = DeleteSchemaResponse'

instance Prelude.NFData DeleteSchemaResponse where
  rnf _ = ()
