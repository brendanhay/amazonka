{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.UpdateRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing registry which is used to hold a collection of
-- schemas. The updated properties relate to the registry, and do not
-- modify any of the schemas within the registry.
module Network.AWS.Glue.UpdateRegistry
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
    updateRegistryResponse_registryName,
    updateRegistryResponse_registryArn,
    updateRegistryResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRegistry' smart constructor.
data UpdateRegistry = UpdateRegistry'
  { -- | This is a wrapper structure that may contain the registry name and
    -- Amazon Resource Name (ARN).
    registryId :: RegistryId,
    -- | A description of the registry. If description is not provided, this
    -- field will not be updated.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateRegistry where
  type Rs UpdateRegistry = UpdateRegistryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegistryResponse'
            Prelude.<$> (x Prelude..?> "RegistryName")
            Prelude.<*> (x Prelude..?> "RegistryArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRegistry

instance Prelude.NFData UpdateRegistry

instance Prelude.ToHeaders UpdateRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateRegistry" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateRegistry where
  toJSON UpdateRegistry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RegistryId" Prelude..= registryId),
            Prelude.Just ("Description" Prelude..= description)
          ]
      )

instance Prelude.ToPath UpdateRegistry where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRegistryResponse' smart constructor.
data UpdateRegistryResponse = UpdateRegistryResponse'
  { -- | The name of the updated registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource name (ARN) of the updated registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'updateRegistryResponse_registryName' - The name of the updated registry.
--
-- 'registryArn', 'updateRegistryResponse_registryArn' - The Amazon Resource name (ARN) of the updated registry.
--
-- 'httpStatus', 'updateRegistryResponse_httpStatus' - The response's http status code.
newUpdateRegistryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRegistryResponse
newUpdateRegistryResponse pHttpStatus_ =
  UpdateRegistryResponse'
    { registryName =
        Prelude.Nothing,
      registryArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the updated registry.
updateRegistryResponse_registryName :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe Prelude.Text)
updateRegistryResponse_registryName = Lens.lens (\UpdateRegistryResponse' {registryName} -> registryName) (\s@UpdateRegistryResponse' {} a -> s {registryName = a} :: UpdateRegistryResponse)

-- | The Amazon Resource name (ARN) of the updated registry.
updateRegistryResponse_registryArn :: Lens.Lens' UpdateRegistryResponse (Prelude.Maybe Prelude.Text)
updateRegistryResponse_registryArn = Lens.lens (\UpdateRegistryResponse' {registryArn} -> registryArn) (\s@UpdateRegistryResponse' {} a -> s {registryArn = a} :: UpdateRegistryResponse)

-- | The response's http status code.
updateRegistryResponse_httpStatus :: Lens.Lens' UpdateRegistryResponse Prelude.Int
updateRegistryResponse_httpStatus = Lens.lens (\UpdateRegistryResponse' {httpStatus} -> httpStatus) (\s@UpdateRegistryResponse' {} a -> s {httpStatus = a} :: UpdateRegistryResponse)

instance Prelude.NFData UpdateRegistryResponse
