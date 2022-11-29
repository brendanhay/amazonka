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
-- Module      : Amazonka.Schemas.DeleteRegistry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Registry.
module Amazonka.Schemas.DeleteRegistry
  ( -- * Creating a Request
    DeleteRegistry (..),
    newDeleteRegistry,

    -- * Request Lenses
    deleteRegistry_registryName,

    -- * Destructuring the Response
    DeleteRegistryResponse (..),
    newDeleteRegistryResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newDeleteRegistry' smart constructor.
data DeleteRegistry = DeleteRegistry'
  { -- | The name of the registry.
    registryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'deleteRegistry_registryName' - The name of the registry.
newDeleteRegistry ::
  -- | 'registryName'
  Prelude.Text ->
  DeleteRegistry
newDeleteRegistry pRegistryName_ =
  DeleteRegistry' {registryName = pRegistryName_}

-- | The name of the registry.
deleteRegistry_registryName :: Lens.Lens' DeleteRegistry Prelude.Text
deleteRegistry_registryName = Lens.lens (\DeleteRegistry' {registryName} -> registryName) (\s@DeleteRegistry' {} a -> s {registryName = a} :: DeleteRegistry)

instance Core.AWSRequest DeleteRegistry where
  type
    AWSResponse DeleteRegistry =
      DeleteRegistryResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRegistryResponse'

instance Prelude.Hashable DeleteRegistry where
  hashWithSalt _salt DeleteRegistry' {..} =
    _salt `Prelude.hashWithSalt` registryName

instance Prelude.NFData DeleteRegistry where
  rnf DeleteRegistry' {..} = Prelude.rnf registryName

instance Core.ToHeaders DeleteRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteRegistry where
  toPath DeleteRegistry' {..} =
    Prelude.mconcat
      ["/v1/registries/name/", Core.toBS registryName]

instance Core.ToQuery DeleteRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRegistryResponse' smart constructor.
data DeleteRegistryResponse = DeleteRegistryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRegistryResponse ::
  DeleteRegistryResponse
newDeleteRegistryResponse = DeleteRegistryResponse'

instance Prelude.NFData DeleteRegistryResponse where
  rnf _ = ()
