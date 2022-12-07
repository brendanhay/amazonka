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
-- Module      : Amazonka.WellArchitected.UpdateShareInvitation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a workload or custom lens share invitation.
--
-- This API operation can be called independently of any resource. Previous
-- documentation implied that a workload ARN must be specified.
module Amazonka.WellArchitected.UpdateShareInvitation
  ( -- * Creating a Request
    UpdateShareInvitation (..),
    newUpdateShareInvitation,

    -- * Request Lenses
    updateShareInvitation_shareInvitationId,
    updateShareInvitation_shareInvitationAction,

    -- * Destructuring the Response
    UpdateShareInvitationResponse (..),
    newUpdateShareInvitationResponse,

    -- * Response Lenses
    updateShareInvitationResponse_shareInvitation,
    updateShareInvitationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for Update Share Invitation
--
-- /See:/ 'newUpdateShareInvitation' smart constructor.
data UpdateShareInvitation = UpdateShareInvitation'
  { -- | The ID assigned to the share invitation.
    shareInvitationId :: Prelude.Text,
    shareInvitationAction :: ShareInvitationAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateShareInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareInvitationId', 'updateShareInvitation_shareInvitationId' - The ID assigned to the share invitation.
--
-- 'shareInvitationAction', 'updateShareInvitation_shareInvitationAction' - Undocumented member.
newUpdateShareInvitation ::
  -- | 'shareInvitationId'
  Prelude.Text ->
  -- | 'shareInvitationAction'
  ShareInvitationAction ->
  UpdateShareInvitation
newUpdateShareInvitation
  pShareInvitationId_
  pShareInvitationAction_ =
    UpdateShareInvitation'
      { shareInvitationId =
          pShareInvitationId_,
        shareInvitationAction = pShareInvitationAction_
      }

-- | The ID assigned to the share invitation.
updateShareInvitation_shareInvitationId :: Lens.Lens' UpdateShareInvitation Prelude.Text
updateShareInvitation_shareInvitationId = Lens.lens (\UpdateShareInvitation' {shareInvitationId} -> shareInvitationId) (\s@UpdateShareInvitation' {} a -> s {shareInvitationId = a} :: UpdateShareInvitation)

-- | Undocumented member.
updateShareInvitation_shareInvitationAction :: Lens.Lens' UpdateShareInvitation ShareInvitationAction
updateShareInvitation_shareInvitationAction = Lens.lens (\UpdateShareInvitation' {shareInvitationAction} -> shareInvitationAction) (\s@UpdateShareInvitation' {} a -> s {shareInvitationAction = a} :: UpdateShareInvitation)

instance Core.AWSRequest UpdateShareInvitation where
  type
    AWSResponse UpdateShareInvitation =
      UpdateShareInvitationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateShareInvitationResponse'
            Prelude.<$> (x Data..?> "ShareInvitation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateShareInvitation where
  hashWithSalt _salt UpdateShareInvitation' {..} =
    _salt `Prelude.hashWithSalt` shareInvitationId
      `Prelude.hashWithSalt` shareInvitationAction

instance Prelude.NFData UpdateShareInvitation where
  rnf UpdateShareInvitation' {..} =
    Prelude.rnf shareInvitationId
      `Prelude.seq` Prelude.rnf shareInvitationAction

instance Data.ToHeaders UpdateShareInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateShareInvitation where
  toJSON UpdateShareInvitation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ShareInvitationAction"
                  Data..= shareInvitationAction
              )
          ]
      )

instance Data.ToPath UpdateShareInvitation where
  toPath UpdateShareInvitation' {..} =
    Prelude.mconcat
      ["/shareInvitations/", Data.toBS shareInvitationId]

instance Data.ToQuery UpdateShareInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateShareInvitationResponse' smart constructor.
data UpdateShareInvitationResponse = UpdateShareInvitationResponse'
  { -- | The updated workload or custom lens share invitation.
    shareInvitation :: Prelude.Maybe ShareInvitation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateShareInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareInvitation', 'updateShareInvitationResponse_shareInvitation' - The updated workload or custom lens share invitation.
--
-- 'httpStatus', 'updateShareInvitationResponse_httpStatus' - The response's http status code.
newUpdateShareInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateShareInvitationResponse
newUpdateShareInvitationResponse pHttpStatus_ =
  UpdateShareInvitationResponse'
    { shareInvitation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated workload or custom lens share invitation.
updateShareInvitationResponse_shareInvitation :: Lens.Lens' UpdateShareInvitationResponse (Prelude.Maybe ShareInvitation)
updateShareInvitationResponse_shareInvitation = Lens.lens (\UpdateShareInvitationResponse' {shareInvitation} -> shareInvitation) (\s@UpdateShareInvitationResponse' {} a -> s {shareInvitation = a} :: UpdateShareInvitationResponse)

-- | The response's http status code.
updateShareInvitationResponse_httpStatus :: Lens.Lens' UpdateShareInvitationResponse Prelude.Int
updateShareInvitationResponse_httpStatus = Lens.lens (\UpdateShareInvitationResponse' {httpStatus} -> httpStatus) (\s@UpdateShareInvitationResponse' {} a -> s {httpStatus = a} :: UpdateShareInvitationResponse)

instance Prelude.NFData UpdateShareInvitationResponse where
  rnf UpdateShareInvitationResponse' {..} =
    Prelude.rnf shareInvitation
      `Prelude.seq` Prelude.rnf httpStatus
