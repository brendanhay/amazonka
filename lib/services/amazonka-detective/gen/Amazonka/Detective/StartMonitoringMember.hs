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
-- Module      : Amazonka.Detective.StartMonitoringMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a request to enable data ingest for a member account that has a
-- status of @ACCEPTED_BUT_DISABLED@.
--
-- For valid member accounts, the status is updated as follows.
--
-- -   If Detective enabled the member account, then the new status is
--     @ENABLED@.
--
-- -   If Detective cannot enable the member account, the status remains
--     @ACCEPTED_BUT_DISABLED@.
module Amazonka.Detective.StartMonitoringMember
  ( -- * Creating a Request
    StartMonitoringMember (..),
    newStartMonitoringMember,

    -- * Request Lenses
    startMonitoringMember_graphArn,
    startMonitoringMember_accountId,

    -- * Destructuring the Response
    StartMonitoringMemberResponse (..),
    newStartMonitoringMemberResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMonitoringMember' smart constructor.
data StartMonitoringMember = StartMonitoringMember'
  { -- | The ARN of the behavior graph.
    graphArn :: Prelude.Text,
    -- | The account ID of the member account to try to enable.
    --
    -- The account must be an invited member account with a status of
    -- @ACCEPTED_BUT_DISABLED@.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMonitoringMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'startMonitoringMember_graphArn' - The ARN of the behavior graph.
--
-- 'accountId', 'startMonitoringMember_accountId' - The account ID of the member account to try to enable.
--
-- The account must be an invited member account with a status of
-- @ACCEPTED_BUT_DISABLED@.
newStartMonitoringMember ::
  -- | 'graphArn'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  StartMonitoringMember
newStartMonitoringMember pGraphArn_ pAccountId_ =
  StartMonitoringMember'
    { graphArn = pGraphArn_,
      accountId = pAccountId_
    }

-- | The ARN of the behavior graph.
startMonitoringMember_graphArn :: Lens.Lens' StartMonitoringMember Prelude.Text
startMonitoringMember_graphArn = Lens.lens (\StartMonitoringMember' {graphArn} -> graphArn) (\s@StartMonitoringMember' {} a -> s {graphArn = a} :: StartMonitoringMember)

-- | The account ID of the member account to try to enable.
--
-- The account must be an invited member account with a status of
-- @ACCEPTED_BUT_DISABLED@.
startMonitoringMember_accountId :: Lens.Lens' StartMonitoringMember Prelude.Text
startMonitoringMember_accountId = Lens.lens (\StartMonitoringMember' {accountId} -> accountId) (\s@StartMonitoringMember' {} a -> s {accountId = a} :: StartMonitoringMember)

instance Core.AWSRequest StartMonitoringMember where
  type
    AWSResponse StartMonitoringMember =
      StartMonitoringMemberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StartMonitoringMemberResponse'

instance Prelude.Hashable StartMonitoringMember where
  hashWithSalt _salt StartMonitoringMember' {..} =
    _salt `Prelude.hashWithSalt` graphArn
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData StartMonitoringMember where
  rnf StartMonitoringMember' {..} =
    Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToHeaders StartMonitoringMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMonitoringMember where
  toJSON StartMonitoringMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GraphArn" Data..= graphArn),
            Prelude.Just ("AccountId" Data..= accountId)
          ]
      )

instance Data.ToPath StartMonitoringMember where
  toPath =
    Prelude.const "/graph/member/monitoringstate"

instance Data.ToQuery StartMonitoringMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartMonitoringMemberResponse' smart constructor.
data StartMonitoringMemberResponse = StartMonitoringMemberResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMonitoringMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartMonitoringMemberResponse ::
  StartMonitoringMemberResponse
newStartMonitoringMemberResponse =
  StartMonitoringMemberResponse'

instance Prelude.NFData StartMonitoringMemberResponse where
  rnf _ = ()
