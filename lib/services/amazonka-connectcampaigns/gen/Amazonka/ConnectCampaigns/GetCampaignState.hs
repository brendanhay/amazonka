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
-- Module      : Amazonka.ConnectCampaigns.GetCampaignState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get state of a campaign for the specified Amazon Connect account.
module Amazonka.ConnectCampaigns.GetCampaignState
  ( -- * Creating a Request
    GetCampaignState (..),
    newGetCampaignState,

    -- * Request Lenses
    getCampaignState_id,

    -- * Destructuring the Response
    GetCampaignStateResponse (..),
    newGetCampaignStateResponse,

    -- * Response Lenses
    getCampaignStateResponse_state,
    getCampaignStateResponse_httpStatus,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | GetCampaignStateRequest
--
-- /See:/ 'newGetCampaignState' smart constructor.
data GetCampaignState = GetCampaignState'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCampaignState_id' - Undocumented member.
newGetCampaignState ::
  -- | 'id'
  Prelude.Text ->
  GetCampaignState
newGetCampaignState pId_ =
  GetCampaignState' {id = pId_}

-- | Undocumented member.
getCampaignState_id :: Lens.Lens' GetCampaignState Prelude.Text
getCampaignState_id = Lens.lens (\GetCampaignState' {id} -> id) (\s@GetCampaignState' {} a -> s {id = a} :: GetCampaignState)

instance Core.AWSRequest GetCampaignState where
  type
    AWSResponse GetCampaignState =
      GetCampaignStateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignStateResponse'
            Prelude.<$> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCampaignState where
  hashWithSalt _salt GetCampaignState' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetCampaignState where
  rnf GetCampaignState' {..} = Prelude.rnf id

instance Data.ToHeaders GetCampaignState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCampaignState where
  toPath GetCampaignState' {..} =
    Prelude.mconcat
      ["/campaigns/", Data.toBS id, "/state"]

instance Data.ToQuery GetCampaignState where
  toQuery = Prelude.const Prelude.mempty

-- | GetCampaignStateResponse
--
-- /See:/ 'newGetCampaignStateResponse' smart constructor.
data GetCampaignStateResponse = GetCampaignStateResponse'
  { state :: Prelude.Maybe CampaignState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'getCampaignStateResponse_state' - Undocumented member.
--
-- 'httpStatus', 'getCampaignStateResponse_httpStatus' - The response's http status code.
newGetCampaignStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCampaignStateResponse
newGetCampaignStateResponse pHttpStatus_ =
  GetCampaignStateResponse'
    { state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getCampaignStateResponse_state :: Lens.Lens' GetCampaignStateResponse (Prelude.Maybe CampaignState)
getCampaignStateResponse_state = Lens.lens (\GetCampaignStateResponse' {state} -> state) (\s@GetCampaignStateResponse' {} a -> s {state = a} :: GetCampaignStateResponse)

-- | The response's http status code.
getCampaignStateResponse_httpStatus :: Lens.Lens' GetCampaignStateResponse Prelude.Int
getCampaignStateResponse_httpStatus = Lens.lens (\GetCampaignStateResponse' {httpStatus} -> httpStatus) (\s@GetCampaignStateResponse' {} a -> s {httpStatus = a} :: GetCampaignStateResponse)

instance Prelude.NFData GetCampaignStateResponse where
  rnf GetCampaignStateResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
