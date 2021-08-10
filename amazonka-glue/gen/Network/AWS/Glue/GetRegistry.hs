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
-- Module      : Network.AWS.Glue.GetRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified registry in detail.
module Network.AWS.Glue.GetRegistry
  ( -- * Creating a Request
    GetRegistry (..),
    newGetRegistry,

    -- * Request Lenses
    getRegistry_registryId,

    -- * Destructuring the Response
    GetRegistryResponse (..),
    newGetRegistryResponse,

    -- * Response Lenses
    getRegistryResponse_status,
    getRegistryResponse_updatedTime,
    getRegistryResponse_createdTime,
    getRegistryResponse_registryName,
    getRegistryResponse_description,
    getRegistryResponse_registryArn,
    getRegistryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRegistry' smart constructor.
data GetRegistry = GetRegistry'
  { -- | This is a wrapper structure that may contain the registry name and
    -- Amazon Resource Name (ARN).
    registryId :: RegistryId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getRegistry_registryId' - This is a wrapper structure that may contain the registry name and
-- Amazon Resource Name (ARN).
newGetRegistry ::
  -- | 'registryId'
  RegistryId ->
  GetRegistry
newGetRegistry pRegistryId_ =
  GetRegistry' {registryId = pRegistryId_}

-- | This is a wrapper structure that may contain the registry name and
-- Amazon Resource Name (ARN).
getRegistry_registryId :: Lens.Lens' GetRegistry RegistryId
getRegistry_registryId = Lens.lens (\GetRegistry' {registryId} -> registryId) (\s@GetRegistry' {} a -> s {registryId = a} :: GetRegistry)

instance Core.AWSRequest GetRegistry where
  type AWSResponse GetRegistry = GetRegistryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistryResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "UpdatedTime")
            Prelude.<*> (x Core..?> "CreatedTime")
            Prelude.<*> (x Core..?> "RegistryName")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RegistryArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegistry

instance Prelude.NFData GetRegistry

instance Core.ToHeaders GetRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetRegistry" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRegistry where
  toJSON GetRegistry' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("RegistryId" Core..= registryId)]
      )

instance Core.ToPath GetRegistry where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegistryResponse' smart constructor.
data GetRegistryResponse = GetRegistryResponse'
  { -- | The status of the registry.
    status :: Prelude.Maybe RegistryStatus,
    -- | The date and time the registry was updated.
    updatedTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time the registry was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | A description of the registry.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getRegistryResponse_status' - The status of the registry.
--
-- 'updatedTime', 'getRegistryResponse_updatedTime' - The date and time the registry was updated.
--
-- 'createdTime', 'getRegistryResponse_createdTime' - The date and time the registry was created.
--
-- 'registryName', 'getRegistryResponse_registryName' - The name of the registry.
--
-- 'description', 'getRegistryResponse_description' - A description of the registry.
--
-- 'registryArn', 'getRegistryResponse_registryArn' - The Amazon Resource Name (ARN) of the registry.
--
-- 'httpStatus', 'getRegistryResponse_httpStatus' - The response's http status code.
newGetRegistryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegistryResponse
newGetRegistryResponse pHttpStatus_ =
  GetRegistryResponse'
    { status = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      registryName = Prelude.Nothing,
      description = Prelude.Nothing,
      registryArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the registry.
getRegistryResponse_status :: Lens.Lens' GetRegistryResponse (Prelude.Maybe RegistryStatus)
getRegistryResponse_status = Lens.lens (\GetRegistryResponse' {status} -> status) (\s@GetRegistryResponse' {} a -> s {status = a} :: GetRegistryResponse)

-- | The date and time the registry was updated.
getRegistryResponse_updatedTime :: Lens.Lens' GetRegistryResponse (Prelude.Maybe Prelude.Text)
getRegistryResponse_updatedTime = Lens.lens (\GetRegistryResponse' {updatedTime} -> updatedTime) (\s@GetRegistryResponse' {} a -> s {updatedTime = a} :: GetRegistryResponse)

-- | The date and time the registry was created.
getRegistryResponse_createdTime :: Lens.Lens' GetRegistryResponse (Prelude.Maybe Prelude.Text)
getRegistryResponse_createdTime = Lens.lens (\GetRegistryResponse' {createdTime} -> createdTime) (\s@GetRegistryResponse' {} a -> s {createdTime = a} :: GetRegistryResponse)

-- | The name of the registry.
getRegistryResponse_registryName :: Lens.Lens' GetRegistryResponse (Prelude.Maybe Prelude.Text)
getRegistryResponse_registryName = Lens.lens (\GetRegistryResponse' {registryName} -> registryName) (\s@GetRegistryResponse' {} a -> s {registryName = a} :: GetRegistryResponse)

-- | A description of the registry.
getRegistryResponse_description :: Lens.Lens' GetRegistryResponse (Prelude.Maybe Prelude.Text)
getRegistryResponse_description = Lens.lens (\GetRegistryResponse' {description} -> description) (\s@GetRegistryResponse' {} a -> s {description = a} :: GetRegistryResponse)

-- | The Amazon Resource Name (ARN) of the registry.
getRegistryResponse_registryArn :: Lens.Lens' GetRegistryResponse (Prelude.Maybe Prelude.Text)
getRegistryResponse_registryArn = Lens.lens (\GetRegistryResponse' {registryArn} -> registryArn) (\s@GetRegistryResponse' {} a -> s {registryArn = a} :: GetRegistryResponse)

-- | The response's http status code.
getRegistryResponse_httpStatus :: Lens.Lens' GetRegistryResponse Prelude.Int
getRegistryResponse_httpStatus = Lens.lens (\GetRegistryResponse' {httpStatus} -> httpStatus) (\s@GetRegistryResponse' {} a -> s {httpStatus = a} :: GetRegistryResponse)

instance Prelude.NFData GetRegistryResponse
