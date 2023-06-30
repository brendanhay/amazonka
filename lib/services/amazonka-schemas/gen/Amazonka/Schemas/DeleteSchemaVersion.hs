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
-- Module      : Amazonka.Schemas.DeleteSchemaVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the schema version definition
module Amazonka.Schemas.DeleteSchemaVersion
  ( -- * Creating a Request
    DeleteSchemaVersion (..),
    newDeleteSchemaVersion,

    -- * Request Lenses
    deleteSchemaVersion_schemaVersion,
    deleteSchemaVersion_registryName,
    deleteSchemaVersion_schemaName,

    -- * Destructuring the Response
    DeleteSchemaVersionResponse (..),
    newDeleteSchemaVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newDeleteSchemaVersion' smart constructor.
data DeleteSchemaVersion = DeleteSchemaVersion'
  { -- | The version number of the schema
    schemaVersion :: Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchemaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersion', 'deleteSchemaVersion_schemaVersion' - The version number of the schema
--
-- 'registryName', 'deleteSchemaVersion_registryName' - The name of the registry.
--
-- 'schemaName', 'deleteSchemaVersion_schemaName' - The name of the schema.
newDeleteSchemaVersion ::
  -- | 'schemaVersion'
  Prelude.Text ->
  -- | 'registryName'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  DeleteSchemaVersion
newDeleteSchemaVersion
  pSchemaVersion_
  pRegistryName_
  pSchemaName_ =
    DeleteSchemaVersion'
      { schemaVersion =
          pSchemaVersion_,
        registryName = pRegistryName_,
        schemaName = pSchemaName_
      }

-- | The version number of the schema
deleteSchemaVersion_schemaVersion :: Lens.Lens' DeleteSchemaVersion Prelude.Text
deleteSchemaVersion_schemaVersion = Lens.lens (\DeleteSchemaVersion' {schemaVersion} -> schemaVersion) (\s@DeleteSchemaVersion' {} a -> s {schemaVersion = a} :: DeleteSchemaVersion)

-- | The name of the registry.
deleteSchemaVersion_registryName :: Lens.Lens' DeleteSchemaVersion Prelude.Text
deleteSchemaVersion_registryName = Lens.lens (\DeleteSchemaVersion' {registryName} -> registryName) (\s@DeleteSchemaVersion' {} a -> s {registryName = a} :: DeleteSchemaVersion)

-- | The name of the schema.
deleteSchemaVersion_schemaName :: Lens.Lens' DeleteSchemaVersion Prelude.Text
deleteSchemaVersion_schemaName = Lens.lens (\DeleteSchemaVersion' {schemaName} -> schemaName) (\s@DeleteSchemaVersion' {} a -> s {schemaName = a} :: DeleteSchemaVersion)

instance Core.AWSRequest DeleteSchemaVersion where
  type
    AWSResponse DeleteSchemaVersion =
      DeleteSchemaVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteSchemaVersionResponse'

instance Prelude.Hashable DeleteSchemaVersion where
  hashWithSalt _salt DeleteSchemaVersion' {..} =
    _salt
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaName

instance Prelude.NFData DeleteSchemaVersion where
  rnf DeleteSchemaVersion' {..} =
    Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName

instance Data.ToHeaders DeleteSchemaVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSchemaVersion where
  toPath DeleteSchemaVersion' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Data.toBS registryName,
        "/schemas/name/",
        Data.toBS schemaName,
        "/version/",
        Data.toBS schemaVersion
      ]

instance Data.ToQuery DeleteSchemaVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSchemaVersionResponse' smart constructor.
data DeleteSchemaVersionResponse = DeleteSchemaVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchemaVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSchemaVersionResponse ::
  DeleteSchemaVersionResponse
newDeleteSchemaVersionResponse =
  DeleteSchemaVersionResponse'

instance Prelude.NFData DeleteSchemaVersionResponse where
  rnf _ = ()
