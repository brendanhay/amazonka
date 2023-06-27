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
-- Module      : Amazonka.MQ.Promote
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes a data replication replica broker to the primary broker role.
module Amazonka.MQ.Promote
  ( -- * Creating a Request
    Promote (..),
    newPromote,

    -- * Request Lenses
    promote_brokerId,
    promote_mode,

    -- * Destructuring the Response
    PromoteResponse (..),
    newPromoteResponse,

    -- * Response Lenses
    promoteResponse_brokerId,
    promoteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Promotes a data replication replica broker to the primary broker role.
--
-- /See:/ 'newPromote' smart constructor.
data Promote = Promote'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text,
    -- | The Promote mode requested. Note: Valid values for the parameter are
    -- SWITCHOVER, FAILOVER.
    mode :: PromoteMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Promote' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerId', 'promote_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'mode', 'promote_mode' - The Promote mode requested. Note: Valid values for the parameter are
-- SWITCHOVER, FAILOVER.
newPromote ::
  -- | 'brokerId'
  Prelude.Text ->
  -- | 'mode'
  PromoteMode ->
  Promote
newPromote pBrokerId_ pMode_ =
  Promote' {brokerId = pBrokerId_, mode = pMode_}

-- | The unique ID that Amazon MQ generates for the broker.
promote_brokerId :: Lens.Lens' Promote Prelude.Text
promote_brokerId = Lens.lens (\Promote' {brokerId} -> brokerId) (\s@Promote' {} a -> s {brokerId = a} :: Promote)

-- | The Promote mode requested. Note: Valid values for the parameter are
-- SWITCHOVER, FAILOVER.
promote_mode :: Lens.Lens' Promote PromoteMode
promote_mode = Lens.lens (\Promote' {mode} -> mode) (\s@Promote' {} a -> s {mode = a} :: Promote)

instance Core.AWSRequest Promote where
  type AWSResponse Promote = PromoteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PromoteResponse'
            Prelude.<$> (x Data..?> "brokerId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Promote where
  hashWithSalt _salt Promote' {..} =
    _salt
      `Prelude.hashWithSalt` brokerId
      `Prelude.hashWithSalt` mode

instance Prelude.NFData Promote where
  rnf Promote' {..} =
    Prelude.rnf brokerId `Prelude.seq` Prelude.rnf mode

instance Data.ToHeaders Promote where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Promote where
  toJSON Promote' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("mode" Data..= mode)]
      )

instance Data.ToPath Promote where
  toPath Promote' {..} =
    Prelude.mconcat
      ["/v1/brokers/", Data.toBS brokerId, "/promote"]

instance Data.ToQuery Promote where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPromoteResponse' smart constructor.
data PromoteResponse = PromoteResponse'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromoteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerId', 'promoteResponse_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'httpStatus', 'promoteResponse_httpStatus' - The response's http status code.
newPromoteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PromoteResponse
newPromoteResponse pHttpStatus_ =
  PromoteResponse'
    { brokerId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID that Amazon MQ generates for the broker.
promoteResponse_brokerId :: Lens.Lens' PromoteResponse (Prelude.Maybe Prelude.Text)
promoteResponse_brokerId = Lens.lens (\PromoteResponse' {brokerId} -> brokerId) (\s@PromoteResponse' {} a -> s {brokerId = a} :: PromoteResponse)

-- | The response's http status code.
promoteResponse_httpStatus :: Lens.Lens' PromoteResponse Prelude.Int
promoteResponse_httpStatus = Lens.lens (\PromoteResponse' {httpStatus} -> httpStatus) (\s@PromoteResponse' {} a -> s {httpStatus = a} :: PromoteResponse)

instance Prelude.NFData PromoteResponse where
  rnf PromoteResponse' {..} =
    Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf httpStatus
