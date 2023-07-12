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
-- Module      : Amazonka.Nimble.UpdateStudio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a Studio resource.
--
-- Currently, this operation only supports updating the displayName of your
-- studio.
module Amazonka.Nimble.UpdateStudio
  ( -- * Creating a Request
    UpdateStudio (..),
    newUpdateStudio,

    -- * Request Lenses
    updateStudio_adminRoleArn,
    updateStudio_clientToken,
    updateStudio_displayName,
    updateStudio_userRoleArn,
    updateStudio_studioId,

    -- * Destructuring the Response
    UpdateStudioResponse (..),
    newUpdateStudioResponse,

    -- * Response Lenses
    updateStudioResponse_httpStatus,
    updateStudioResponse_studio,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStudio' smart constructor.
data UpdateStudio = UpdateStudio'
  { -- | The IAM role that Studio Admins will assume when logging in to the
    -- Nimble Studio portal.
    adminRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A friendly name for the studio.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The IAM role that Studio Users will assume when logging in to the Nimble
    -- Studio portal.
    userRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminRoleArn', 'updateStudio_adminRoleArn' - The IAM role that Studio Admins will assume when logging in to the
-- Nimble Studio portal.
--
-- 'clientToken', 'updateStudio_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'displayName', 'updateStudio_displayName' - A friendly name for the studio.
--
-- 'userRoleArn', 'updateStudio_userRoleArn' - The IAM role that Studio Users will assume when logging in to the Nimble
-- Studio portal.
--
-- 'studioId', 'updateStudio_studioId' - The studio ID.
newUpdateStudio ::
  -- | 'studioId'
  Prelude.Text ->
  UpdateStudio
newUpdateStudio pStudioId_ =
  UpdateStudio'
    { adminRoleArn = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      displayName = Prelude.Nothing,
      userRoleArn = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The IAM role that Studio Admins will assume when logging in to the
-- Nimble Studio portal.
updateStudio_adminRoleArn :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_adminRoleArn = Lens.lens (\UpdateStudio' {adminRoleArn} -> adminRoleArn) (\s@UpdateStudio' {} a -> s {adminRoleArn = a} :: UpdateStudio)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
updateStudio_clientToken :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_clientToken = Lens.lens (\UpdateStudio' {clientToken} -> clientToken) (\s@UpdateStudio' {} a -> s {clientToken = a} :: UpdateStudio)

-- | A friendly name for the studio.
updateStudio_displayName :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_displayName = Lens.lens (\UpdateStudio' {displayName} -> displayName) (\s@UpdateStudio' {} a -> s {displayName = a} :: UpdateStudio) Prelude.. Lens.mapping Data._Sensitive

-- | The IAM role that Studio Users will assume when logging in to the Nimble
-- Studio portal.
updateStudio_userRoleArn :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_userRoleArn = Lens.lens (\UpdateStudio' {userRoleArn} -> userRoleArn) (\s@UpdateStudio' {} a -> s {userRoleArn = a} :: UpdateStudio)

-- | The studio ID.
updateStudio_studioId :: Lens.Lens' UpdateStudio Prelude.Text
updateStudio_studioId = Lens.lens (\UpdateStudio' {studioId} -> studioId) (\s@UpdateStudio' {} a -> s {studioId = a} :: UpdateStudio)

instance Core.AWSRequest UpdateStudio where
  type AWSResponse UpdateStudio = UpdateStudioResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStudioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "studio")
      )

instance Prelude.Hashable UpdateStudio where
  hashWithSalt _salt UpdateStudio' {..} =
    _salt
      `Prelude.hashWithSalt` adminRoleArn
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` userRoleArn
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData UpdateStudio where
  rnf UpdateStudio' {..} =
    Prelude.rnf adminRoleArn
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf userRoleArn
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders UpdateStudio where
  toHeaders UpdateStudio' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON UpdateStudio where
  toJSON UpdateStudio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adminRoleArn" Data..=) Prelude.<$> adminRoleArn,
            ("displayName" Data..=) Prelude.<$> displayName,
            ("userRoleArn" Data..=) Prelude.<$> userRoleArn
          ]
      )

instance Data.ToPath UpdateStudio where
  toPath UpdateStudio' {..} =
    Prelude.mconcat
      ["/2020-08-01/studios/", Data.toBS studioId]

instance Data.ToQuery UpdateStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioResponse' smart constructor.
data UpdateStudioResponse = UpdateStudioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about a studio.
    studio :: Studio
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateStudioResponse_httpStatus' - The response's http status code.
--
-- 'studio', 'updateStudioResponse_studio' - Information about a studio.
newUpdateStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'studio'
  Studio ->
  UpdateStudioResponse
newUpdateStudioResponse pHttpStatus_ pStudio_ =
  UpdateStudioResponse'
    { httpStatus = pHttpStatus_,
      studio = pStudio_
    }

-- | The response's http status code.
updateStudioResponse_httpStatus :: Lens.Lens' UpdateStudioResponse Prelude.Int
updateStudioResponse_httpStatus = Lens.lens (\UpdateStudioResponse' {httpStatus} -> httpStatus) (\s@UpdateStudioResponse' {} a -> s {httpStatus = a} :: UpdateStudioResponse)

-- | Information about a studio.
updateStudioResponse_studio :: Lens.Lens' UpdateStudioResponse Studio
updateStudioResponse_studio = Lens.lens (\UpdateStudioResponse' {studio} -> studio) (\s@UpdateStudioResponse' {} a -> s {studio = a} :: UpdateStudioResponse)

instance Prelude.NFData UpdateStudioResponse where
  rnf UpdateStudioResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf studio
