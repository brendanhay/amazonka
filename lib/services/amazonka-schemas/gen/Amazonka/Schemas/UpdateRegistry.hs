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
-- Module      : Amazonka.Schemas.UpdateRegistry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registry.
module Amazonka.Schemas.UpdateRegistry
  ( -- * Creating a Request
    UpdateRegistry (..),
    newUpdateRegistry,

    -- * Request Lenses
    updateRegistry_description,
    updateRegistry_registryName,

    -- * Destructuring the Response
    UpdateRegistryResponse (..),
    newUpdateRegistryResponse,

    -- * Response Lenses
    updateRegistryResponse_description,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_registryName,
    updateRegistryResponse_tags,
    updateRegistryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | Updates the registry.
--
-- /See:/ 'newUpdateRegistry' smart constructor.
data UpdateRegistry = UpdateRegistry'
  { -- | The description of the registry to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Text
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
-- 'description', 'updateRegistry_description' - The description of the registry to update.
--
-- 'registryName', 'updateRegistry_registryName' - The name of the registry.
newUpdateRegistry ::
  -- | 'registryName'
  Prelude.Text ->
  UpdateRegistry
newUpdateRegistry pRegistryName_ =
  UpdateRegistry'
    { description = Prelude.Nothing,
      registryName = pRegistryName_
    }

-- | The description of the registry to update.
updateRegistry_description :: Lens.Lens' UpdateRegistry (Prelude.Maybe Prelude.Text)
updateRegistry_description = Lens.lens (\UpdateRegistry' {description} -> description) (\s@UpdateRegistry' {} a -> s {description = a} :: UpdateRegistry)

-- | The name of the registry.
updateRegistry_registryName :: Lens.Lens' UpdateRegistry Prelude.Text
updateRegistry_registryName = Lens.lens (\UpdateRegistry' {registryName} -> registryName) (\s@UpdateRegistry' {} a -> s {registryName = a} :: UpdateRegistry)

instance Core.AWSRequest UpdateRegistry where
  type
    AWSResponse UpdateRegistry =
      UpdateRegistryResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegistryResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "RegistryArn")
            Prelude.<*> (x Data..?> "RegistryName")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRegistry where
  hashWithSalt _salt UpdateRegistry' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` registryName

instance Prelude.NFData UpdateRegistry where
  rnf UpdateRegistry' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf registryName

instance Data.ToHeaders UpdateRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRegistry where
  toJSON UpdateRegistry' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateRegistry where
  toPath UpdateRegistry' {..} =
    Prelude.mconcat
      ["/v1/registries/name/", Data.toBS registryName]

instance Data.ToQuery UpdateRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRegistryResponse' smart constructor.
data UpdateRegistryResponse = UpdateRegistryResponse'
  { -- | The description of the registry.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the registry.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'description', 'updateRegistryResponse_description' - The description of the registry.
--
-- 'registryArn', 'updateRegistryResponse_registryArn' - The ARN of the registry.
--
-- 'registryName', 'updateRegistryResponse_registryName' - The name of the registry.
--
-- 'tags', 'updateRegistryResponse_tags' - Tags associated with the registry.
--
-- 'httpStatus', 'updateRegistryResponse_httpStatus' - The response's http status code.
newUpdateRegistryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRegistryResponse
newUpdateRegistryResponse pHttpStatus_ =
  UpdateRegistryResponse'
    { description =
        Prelude.Nothing,
      registryArn = Prelude.Nothing,
      registryName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the registry.
updateRegistryResponse_description :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe Prelude.Text)
updateRegistryResponse_description = Lens.lens (\UpdateRegistryResponse' {description} -> description) (\s@UpdateRegistryResponse' {} a -> s {description = a} :: UpdateRegistryResponse)

-- | The ARN of the registry.
updateRegistryResponse_registryArn :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe Prelude.Text)
updateRegistryResponse_registryArn = Lens.lens (\UpdateRegistryResponse' {registryArn} -> registryArn) (\s@UpdateRegistryResponse' {} a -> s {registryArn = a} :: UpdateRegistryResponse)

-- | The name of the registry.
updateRegistryResponse_registryName :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe Prelude.Text)
updateRegistryResponse_registryName = Lens.lens (\UpdateRegistryResponse' {registryName} -> registryName) (\s@UpdateRegistryResponse' {} a -> s {registryName = a} :: UpdateRegistryResponse)

-- | Tags associated with the registry.
updateRegistryResponse_tags :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateRegistryResponse_tags = Lens.lens (\UpdateRegistryResponse' {tags} -> tags) (\s@UpdateRegistryResponse' {} a -> s {tags = a} :: UpdateRegistryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateRegistryResponse_httpStatus :: Lens.Lens' UpdateRegistryResponse Prelude.Int
updateRegistryResponse_httpStatus = Lens.lens (\UpdateRegistryResponse' {httpStatus} -> httpStatus) (\s@UpdateRegistryResponse' {} a -> s {httpStatus = a} :: UpdateRegistryResponse)

instance Prelude.NFData UpdateRegistryResponse where
  rnf UpdateRegistryResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
