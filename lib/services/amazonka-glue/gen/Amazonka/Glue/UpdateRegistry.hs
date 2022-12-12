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
-- Module      : Amazonka.Glue.UpdateRegistry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing registry which is used to hold a collection of
-- schemas. The updated properties relate to the registry, and do not
-- modify any of the schemas within the registry.
module Amazonka.Glue.UpdateRegistry
  ( -- * Creating a Request
    UpdateRegistry (..),
    newUpdateRegistry,

    -- * Request Lenses
    updateRegistry_registryId,
    updateRegistry_description,

    -- * Destructuring the Response
    UpdateRegistryResponse (..),
    newUpdateRegistryResponse,

    -- * Response Lenses
    updateRegistryResponse_registryArn,
    updateRegistryResponse_registryName,
    updateRegistryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRegistry' smart constructor.
data UpdateRegistry = UpdateRegistry'
  { -- | This is a wrapper structure that may contain the registry name and
    -- Amazon Resource Name (ARN).
    registryId :: RegistryId,
    -- | A description of the registry. If description is not provided, this
    -- field will not be updated.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'updateRegistry_registryId' - This is a wrapper structure that may contain the registry name and
-- Amazon Resource Name (ARN).
--
-- 'description', 'updateRegistry_description' - A description of the registry. If description is not provided, this
-- field will not be updated.
newUpdateRegistry ::
  -- | 'registryId'
  RegistryId ->
  -- | 'description'
  Prelude.Text ->
  UpdateRegistry
newUpdateRegistry pRegistryId_ pDescription_ =
  UpdateRegistry'
    { registryId = pRegistryId_,
      description = pDescription_
    }

-- | This is a wrapper structure that may contain the registry name and
-- Amazon Resource Name (ARN).
updateRegistry_registryId :: Lens.Lens' UpdateRegistry RegistryId
updateRegistry_registryId = Lens.lens (\UpdateRegistry' {registryId} -> registryId) (\s@UpdateRegistry' {} a -> s {registryId = a} :: UpdateRegistry)

-- | A description of the registry. If description is not provided, this
-- field will not be updated.
updateRegistry_description :: Lens.Lens' UpdateRegistry Prelude.Text
updateRegistry_description = Lens.lens (\UpdateRegistry' {description} -> description) (\s@UpdateRegistry' {} a -> s {description = a} :: UpdateRegistry)

instance Core.AWSRequest UpdateRegistry where
  type
    AWSResponse UpdateRegistry =
      UpdateRegistryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegistryResponse'
            Prelude.<$> (x Data..?> "RegistryArn")
            Prelude.<*> (x Data..?> "RegistryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRegistry where
  hashWithSalt _salt UpdateRegistry' {..} =
    _salt `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` description

instance Prelude.NFData UpdateRegistry where
  rnf UpdateRegistry' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf description

instance Data.ToHeaders UpdateRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateRegistry" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRegistry where
  toJSON UpdateRegistry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RegistryId" Data..= registryId),
            Prelude.Just ("Description" Data..= description)
          ]
      )

instance Data.ToPath UpdateRegistry where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRegistryResponse' smart constructor.
data UpdateRegistryResponse = UpdateRegistryResponse'
  { -- | The Amazon Resource name (ARN) of the updated registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the updated registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryArn', 'updateRegistryResponse_registryArn' - The Amazon Resource name (ARN) of the updated registry.
--
-- 'registryName', 'updateRegistryResponse_registryName' - The name of the updated registry.
--
-- 'httpStatus', 'updateRegistryResponse_httpStatus' - The response's http status code.
newUpdateRegistryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRegistryResponse
newUpdateRegistryResponse pHttpStatus_ =
  UpdateRegistryResponse'
    { registryArn =
        Prelude.Nothing,
      registryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource name (ARN) of the updated registry.
updateRegistryResponse_registryArn :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe Prelude.Text)
updateRegistryResponse_registryArn = Lens.lens (\UpdateRegistryResponse' {registryArn} -> registryArn) (\s@UpdateRegistryResponse' {} a -> s {registryArn = a} :: UpdateRegistryResponse)

-- | The name of the updated registry.
updateRegistryResponse_registryName :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe Prelude.Text)
updateRegistryResponse_registryName = Lens.lens (\UpdateRegistryResponse' {registryName} -> registryName) (\s@UpdateRegistryResponse' {} a -> s {registryName = a} :: UpdateRegistryResponse)

-- | The response's http status code.
updateRegistryResponse_httpStatus :: Lens.Lens' UpdateRegistryResponse Prelude.Int
updateRegistryResponse_httpStatus = Lens.lens (\UpdateRegistryResponse' {httpStatus} -> httpStatus) (\s@UpdateRegistryResponse' {} a -> s {httpStatus = a} :: UpdateRegistryResponse)

instance Prelude.NFData UpdateRegistryResponse where
  rnf UpdateRegistryResponse' {..} =
    Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf httpStatus
