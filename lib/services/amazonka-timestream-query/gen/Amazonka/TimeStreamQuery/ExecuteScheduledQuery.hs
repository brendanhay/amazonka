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
-- Module      : Amazonka.TimeStreamQuery.ExecuteScheduledQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this API to run a scheduled query manually.
module Amazonka.TimeStreamQuery.ExecuteScheduledQuery
  ( -- * Creating a Request
    ExecuteScheduledQuery (..),
    newExecuteScheduledQuery,

    -- * Request Lenses
    executeScheduledQuery_clientToken,
    executeScheduledQuery_scheduledQueryArn,
    executeScheduledQuery_invocationTime,

    -- * Destructuring the Response
    ExecuteScheduledQueryResponse (..),
    newExecuteScheduledQueryResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newExecuteScheduledQuery' smart constructor.
data ExecuteScheduledQuery = ExecuteScheduledQuery'
  { -- | Not used.
    clientToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | ARN of the scheduled query.
    scheduledQueryArn :: Prelude.Text,
    -- | The timestamp in UTC. Query will be run as if it was invoked at this
    -- timestamp.
    invocationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteScheduledQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'executeScheduledQuery_clientToken' - Not used.
--
-- 'scheduledQueryArn', 'executeScheduledQuery_scheduledQueryArn' - ARN of the scheduled query.
--
-- 'invocationTime', 'executeScheduledQuery_invocationTime' - The timestamp in UTC. Query will be run as if it was invoked at this
-- timestamp.
newExecuteScheduledQuery ::
  -- | 'scheduledQueryArn'
  Prelude.Text ->
  -- | 'invocationTime'
  Prelude.UTCTime ->
  ExecuteScheduledQuery
newExecuteScheduledQuery
  pScheduledQueryArn_
  pInvocationTime_ =
    ExecuteScheduledQuery'
      { clientToken =
          Prelude.Nothing,
        scheduledQueryArn = pScheduledQueryArn_,
        invocationTime = Data._Time Lens.# pInvocationTime_
      }

-- | Not used.
executeScheduledQuery_clientToken :: Lens.Lens' ExecuteScheduledQuery (Prelude.Maybe Prelude.Text)
executeScheduledQuery_clientToken = Lens.lens (\ExecuteScheduledQuery' {clientToken} -> clientToken) (\s@ExecuteScheduledQuery' {} a -> s {clientToken = a} :: ExecuteScheduledQuery) Prelude.. Lens.mapping Data._Sensitive

-- | ARN of the scheduled query.
executeScheduledQuery_scheduledQueryArn :: Lens.Lens' ExecuteScheduledQuery Prelude.Text
executeScheduledQuery_scheduledQueryArn = Lens.lens (\ExecuteScheduledQuery' {scheduledQueryArn} -> scheduledQueryArn) (\s@ExecuteScheduledQuery' {} a -> s {scheduledQueryArn = a} :: ExecuteScheduledQuery)

-- | The timestamp in UTC. Query will be run as if it was invoked at this
-- timestamp.
executeScheduledQuery_invocationTime :: Lens.Lens' ExecuteScheduledQuery Prelude.UTCTime
executeScheduledQuery_invocationTime = Lens.lens (\ExecuteScheduledQuery' {invocationTime} -> invocationTime) (\s@ExecuteScheduledQuery' {} a -> s {invocationTime = a} :: ExecuteScheduledQuery) Prelude.. Data._Time

instance Core.AWSRequest ExecuteScheduledQuery where
  type
    AWSResponse ExecuteScheduledQuery =
      ExecuteScheduledQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull ExecuteScheduledQueryResponse'

instance Prelude.Hashable ExecuteScheduledQuery where
  hashWithSalt _salt ExecuteScheduledQuery' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` scheduledQueryArn
      `Prelude.hashWithSalt` invocationTime

instance Prelude.NFData ExecuteScheduledQuery where
  rnf ExecuteScheduledQuery' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf scheduledQueryArn `Prelude.seq`
        Prelude.rnf invocationTime

instance Data.ToHeaders ExecuteScheduledQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.ExecuteScheduledQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExecuteScheduledQuery where
  toJSON ExecuteScheduledQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("ScheduledQueryArn" Data..= scheduledQueryArn),
            Prelude.Just
              ("InvocationTime" Data..= invocationTime)
          ]
      )

instance Data.ToPath ExecuteScheduledQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery ExecuteScheduledQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteScheduledQueryResponse' smart constructor.
data ExecuteScheduledQueryResponse = ExecuteScheduledQueryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteScheduledQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newExecuteScheduledQueryResponse ::
  ExecuteScheduledQueryResponse
newExecuteScheduledQueryResponse =
  ExecuteScheduledQueryResponse'

instance Prelude.NFData ExecuteScheduledQueryResponse where
  rnf _ = ()
