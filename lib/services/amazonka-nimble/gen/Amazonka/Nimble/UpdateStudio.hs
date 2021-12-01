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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    updateStudio_clientToken,
    updateStudio_userRoleArn,
    updateStudio_displayName,
    updateStudio_adminRoleArn,
    updateStudio_studioId,

    -- * Destructuring the Response
    UpdateStudioResponse (..),
    newUpdateStudioResponse,

    -- * Response Lenses
    updateStudioResponse_studio,
    updateStudioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The studio ID.
--
-- /See:/ 'newUpdateStudio' smart constructor.
data UpdateStudio = UpdateStudio'
  { -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that Studio Users will assume when logging in to the Nimble
    -- Studio portal.
    userRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A friendly name for the studio.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that Studio Admins will assume when logging in to the
    -- Nimble Studio portal.
    adminRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateStudio_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'userRoleArn', 'updateStudio_userRoleArn' - The IAM role that Studio Users will assume when logging in to the Nimble
-- Studio portal.
--
-- 'displayName', 'updateStudio_displayName' - A friendly name for the studio.
--
-- 'adminRoleArn', 'updateStudio_adminRoleArn' - The IAM role that Studio Admins will assume when logging in to the
-- Nimble Studio portal.
--
-- 'studioId', 'updateStudio_studioId' - The studio ID.
newUpdateStudio ::
  -- | 'studioId'
  Prelude.Text ->
  UpdateStudio
newUpdateStudio pStudioId_ =
  UpdateStudio'
    { clientToken = Prelude.Nothing,
      userRoleArn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      adminRoleArn = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
updateStudio_clientToken :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_clientToken = Lens.lens (\UpdateStudio' {clientToken} -> clientToken) (\s@UpdateStudio' {} a -> s {clientToken = a} :: UpdateStudio)

-- | The IAM role that Studio Users will assume when logging in to the Nimble
-- Studio portal.
updateStudio_userRoleArn :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_userRoleArn = Lens.lens (\UpdateStudio' {userRoleArn} -> userRoleArn) (\s@UpdateStudio' {} a -> s {userRoleArn = a} :: UpdateStudio)

-- | A friendly name for the studio.
updateStudio_displayName :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_displayName = Lens.lens (\UpdateStudio' {displayName} -> displayName) (\s@UpdateStudio' {} a -> s {displayName = a} :: UpdateStudio)

-- | The IAM role that Studio Admins will assume when logging in to the
-- Nimble Studio portal.
updateStudio_adminRoleArn :: Lens.Lens' UpdateStudio (Prelude.Maybe Prelude.Text)
updateStudio_adminRoleArn = Lens.lens (\UpdateStudio' {adminRoleArn} -> adminRoleArn) (\s@UpdateStudio' {} a -> s {adminRoleArn = a} :: UpdateStudio)

-- | The studio ID.
updateStudio_studioId :: Lens.Lens' UpdateStudio Prelude.Text
updateStudio_studioId = Lens.lens (\UpdateStudio' {studioId} -> studioId) (\s@UpdateStudio' {} a -> s {studioId = a} :: UpdateStudio)

instance Core.AWSRequest UpdateStudio where
  type AWSResponse UpdateStudio = UpdateStudioResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStudioResponse'
            Prelude.<$> (x Core..?> "studio")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStudio where
  hashWithSalt salt' UpdateStudio' {..} =
    salt' `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` adminRoleArn
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` userRoleArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData UpdateStudio where
  rnf UpdateStudio' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf adminRoleArn
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf userRoleArn

instance Core.ToHeaders UpdateStudio where
  toHeaders UpdateStudio' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON UpdateStudio where
  toJSON UpdateStudio' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("userRoleArn" Core..=) Prelude.<$> userRoleArn,
            ("displayName" Core..=) Prelude.<$> displayName,
            ("adminRoleArn" Core..=) Prelude.<$> adminRoleArn
          ]
      )

instance Core.ToPath UpdateStudio where
  toPath UpdateStudio' {..} =
    Prelude.mconcat
      ["/2020-08-01/studios/", Core.toBS studioId]

instance Core.ToQuery UpdateStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioResponse' smart constructor.
data UpdateStudioResponse = UpdateStudioResponse'
  { -- | Information about a studio.
    studio :: Prelude.Maybe Studio,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studio', 'updateStudioResponse_studio' - Information about a studio.
--
-- 'httpStatus', 'updateStudioResponse_httpStatus' - The response's http status code.
newUpdateStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStudioResponse
newUpdateStudioResponse pHttpStatus_ =
  UpdateStudioResponse'
    { studio = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a studio.
updateStudioResponse_studio :: Lens.Lens' UpdateStudioResponse (Prelude.Maybe Studio)
updateStudioResponse_studio = Lens.lens (\UpdateStudioResponse' {studio} -> studio) (\s@UpdateStudioResponse' {} a -> s {studio = a} :: UpdateStudioResponse)

-- | The response's http status code.
updateStudioResponse_httpStatus :: Lens.Lens' UpdateStudioResponse Prelude.Int
updateStudioResponse_httpStatus = Lens.lens (\UpdateStudioResponse' {httpStatus} -> httpStatus) (\s@UpdateStudioResponse' {} a -> s {httpStatus = a} :: UpdateStudioResponse)

instance Prelude.NFData UpdateStudioResponse where
  rnf UpdateStudioResponse' {..} =
    Prelude.rnf studio
      `Prelude.seq` Prelude.rnf httpStatus
