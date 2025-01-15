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
-- Module      : Amazonka.TimeStreamQuery.UpdateScheduledQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a scheduled query.
module Amazonka.TimeStreamQuery.UpdateScheduledQuery
  ( -- * Creating a Request
    UpdateScheduledQuery (..),
    newUpdateScheduledQuery,

    -- * Request Lenses
    updateScheduledQuery_scheduledQueryArn,
    updateScheduledQuery_state,

    -- * Destructuring the Response
    UpdateScheduledQueryResponse (..),
    newUpdateScheduledQueryResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newUpdateScheduledQuery' smart constructor.
data UpdateScheduledQuery = UpdateScheduledQuery'
  { -- | ARN of the scheuled query.
    scheduledQueryArn :: Prelude.Text,
    -- | State of the scheduled query.
    state :: ScheduledQueryState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScheduledQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledQueryArn', 'updateScheduledQuery_scheduledQueryArn' - ARN of the scheuled query.
--
-- 'state', 'updateScheduledQuery_state' - State of the scheduled query.
newUpdateScheduledQuery ::
  -- | 'scheduledQueryArn'
  Prelude.Text ->
  -- | 'state'
  ScheduledQueryState ->
  UpdateScheduledQuery
newUpdateScheduledQuery pScheduledQueryArn_ pState_ =
  UpdateScheduledQuery'
    { scheduledQueryArn =
        pScheduledQueryArn_,
      state = pState_
    }

-- | ARN of the scheuled query.
updateScheduledQuery_scheduledQueryArn :: Lens.Lens' UpdateScheduledQuery Prelude.Text
updateScheduledQuery_scheduledQueryArn = Lens.lens (\UpdateScheduledQuery' {scheduledQueryArn} -> scheduledQueryArn) (\s@UpdateScheduledQuery' {} a -> s {scheduledQueryArn = a} :: UpdateScheduledQuery)

-- | State of the scheduled query.
updateScheduledQuery_state :: Lens.Lens' UpdateScheduledQuery ScheduledQueryState
updateScheduledQuery_state = Lens.lens (\UpdateScheduledQuery' {state} -> state) (\s@UpdateScheduledQuery' {} a -> s {state = a} :: UpdateScheduledQuery)

instance Core.AWSRequest UpdateScheduledQuery where
  type
    AWSResponse UpdateScheduledQuery =
      UpdateScheduledQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateScheduledQueryResponse'

instance Prelude.Hashable UpdateScheduledQuery where
  hashWithSalt _salt UpdateScheduledQuery' {..} =
    _salt
      `Prelude.hashWithSalt` scheduledQueryArn
      `Prelude.hashWithSalt` state

instance Prelude.NFData UpdateScheduledQuery where
  rnf UpdateScheduledQuery' {..} =
    Prelude.rnf scheduledQueryArn `Prelude.seq`
      Prelude.rnf state

instance Data.ToHeaders UpdateScheduledQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.UpdateScheduledQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateScheduledQuery where
  toJSON UpdateScheduledQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScheduledQueryArn" Data..= scheduledQueryArn),
            Prelude.Just ("State" Data..= state)
          ]
      )

instance Data.ToPath UpdateScheduledQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateScheduledQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateScheduledQueryResponse' smart constructor.
data UpdateScheduledQueryResponse = UpdateScheduledQueryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScheduledQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateScheduledQueryResponse ::
  UpdateScheduledQueryResponse
newUpdateScheduledQueryResponse =
  UpdateScheduledQueryResponse'

instance Prelude.NFData UpdateScheduledQueryResponse where
  rnf _ = ()
