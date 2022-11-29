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
-- Module      : Amazonka.IoTWireless.CancelMulticastGroupSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an existing multicast group session.
module Amazonka.IoTWireless.CancelMulticastGroupSession
  ( -- * Creating a Request
    CancelMulticastGroupSession (..),
    newCancelMulticastGroupSession,

    -- * Request Lenses
    cancelMulticastGroupSession_id,

    -- * Destructuring the Response
    CancelMulticastGroupSessionResponse (..),
    newCancelMulticastGroupSessionResponse,

    -- * Response Lenses
    cancelMulticastGroupSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelMulticastGroupSession' smart constructor.
data CancelMulticastGroupSession = CancelMulticastGroupSession'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelMulticastGroupSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'cancelMulticastGroupSession_id' - Undocumented member.
newCancelMulticastGroupSession ::
  -- | 'id'
  Prelude.Text ->
  CancelMulticastGroupSession
newCancelMulticastGroupSession pId_ =
  CancelMulticastGroupSession' {id = pId_}

-- | Undocumented member.
cancelMulticastGroupSession_id :: Lens.Lens' CancelMulticastGroupSession Prelude.Text
cancelMulticastGroupSession_id = Lens.lens (\CancelMulticastGroupSession' {id} -> id) (\s@CancelMulticastGroupSession' {} a -> s {id = a} :: CancelMulticastGroupSession)

instance Core.AWSRequest CancelMulticastGroupSession where
  type
    AWSResponse CancelMulticastGroupSession =
      CancelMulticastGroupSessionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelMulticastGroupSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelMulticastGroupSession where
  hashWithSalt _salt CancelMulticastGroupSession' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData CancelMulticastGroupSession where
  rnf CancelMulticastGroupSession' {..} = Prelude.rnf id

instance Core.ToHeaders CancelMulticastGroupSession where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CancelMulticastGroupSession where
  toPath CancelMulticastGroupSession' {..} =
    Prelude.mconcat
      ["/multicast-groups/", Core.toBS id, "/session"]

instance Core.ToQuery CancelMulticastGroupSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelMulticastGroupSessionResponse' smart constructor.
data CancelMulticastGroupSessionResponse = CancelMulticastGroupSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelMulticastGroupSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelMulticastGroupSessionResponse_httpStatus' - The response's http status code.
newCancelMulticastGroupSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelMulticastGroupSessionResponse
newCancelMulticastGroupSessionResponse pHttpStatus_ =
  CancelMulticastGroupSessionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelMulticastGroupSessionResponse_httpStatus :: Lens.Lens' CancelMulticastGroupSessionResponse Prelude.Int
cancelMulticastGroupSessionResponse_httpStatus = Lens.lens (\CancelMulticastGroupSessionResponse' {httpStatus} -> httpStatus) (\s@CancelMulticastGroupSessionResponse' {} a -> s {httpStatus = a} :: CancelMulticastGroupSessionResponse)

instance
  Prelude.NFData
    CancelMulticastGroupSessionResponse
  where
  rnf CancelMulticastGroupSessionResponse' {..} =
    Prelude.rnf httpStatus
