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
-- Module      : Amazonka.DrS.ReverseReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start replication to origin \/ target region - applies only to protected
-- instances that originated in EC2. For recovery instances on target
-- region - starts replication back to origin region. For failback
-- instances on origin region - starts replication to target region to
-- re-protect them.
module Amazonka.DrS.ReverseReplication
  ( -- * Creating a Request
    ReverseReplication (..),
    newReverseReplication,

    -- * Request Lenses
    reverseReplication_recoveryInstanceID,

    -- * Destructuring the Response
    ReverseReplicationResponse (..),
    newReverseReplicationResponse,

    -- * Response Lenses
    reverseReplicationResponse_reversedDirectionSourceServerArn,
    reverseReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReverseReplication' smart constructor.
data ReverseReplication = ReverseReplication'
  { -- | The ID of the Recovery Instance that we want to reverse the replication
    -- for.
    recoveryInstanceID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReverseReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryInstanceID', 'reverseReplication_recoveryInstanceID' - The ID of the Recovery Instance that we want to reverse the replication
-- for.
newReverseReplication ::
  -- | 'recoveryInstanceID'
  Prelude.Text ->
  ReverseReplication
newReverseReplication pRecoveryInstanceID_ =
  ReverseReplication'
    { recoveryInstanceID =
        pRecoveryInstanceID_
    }

-- | The ID of the Recovery Instance that we want to reverse the replication
-- for.
reverseReplication_recoveryInstanceID :: Lens.Lens' ReverseReplication Prelude.Text
reverseReplication_recoveryInstanceID = Lens.lens (\ReverseReplication' {recoveryInstanceID} -> recoveryInstanceID) (\s@ReverseReplication' {} a -> s {recoveryInstanceID = a} :: ReverseReplication)

instance Core.AWSRequest ReverseReplication where
  type
    AWSResponse ReverseReplication =
      ReverseReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReverseReplicationResponse'
            Prelude.<$> (x Data..?> "reversedDirectionSourceServerArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReverseReplication where
  hashWithSalt _salt ReverseReplication' {..} =
    _salt `Prelude.hashWithSalt` recoveryInstanceID

instance Prelude.NFData ReverseReplication where
  rnf ReverseReplication' {..} =
    Prelude.rnf recoveryInstanceID

instance Data.ToHeaders ReverseReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ReverseReplication where
  toJSON ReverseReplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recoveryInstanceID" Data..= recoveryInstanceID)
          ]
      )

instance Data.ToPath ReverseReplication where
  toPath = Prelude.const "/ReverseReplication"

instance Data.ToQuery ReverseReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReverseReplicationResponse' smart constructor.
data ReverseReplicationResponse = ReverseReplicationResponse'
  { -- | ARN of created SourceServer.
    reversedDirectionSourceServerArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReverseReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reversedDirectionSourceServerArn', 'reverseReplicationResponse_reversedDirectionSourceServerArn' - ARN of created SourceServer.
--
-- 'httpStatus', 'reverseReplicationResponse_httpStatus' - The response's http status code.
newReverseReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReverseReplicationResponse
newReverseReplicationResponse pHttpStatus_ =
  ReverseReplicationResponse'
    { reversedDirectionSourceServerArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ARN of created SourceServer.
reverseReplicationResponse_reversedDirectionSourceServerArn :: Lens.Lens' ReverseReplicationResponse (Prelude.Maybe Prelude.Text)
reverseReplicationResponse_reversedDirectionSourceServerArn = Lens.lens (\ReverseReplicationResponse' {reversedDirectionSourceServerArn} -> reversedDirectionSourceServerArn) (\s@ReverseReplicationResponse' {} a -> s {reversedDirectionSourceServerArn = a} :: ReverseReplicationResponse)

-- | The response's http status code.
reverseReplicationResponse_httpStatus :: Lens.Lens' ReverseReplicationResponse Prelude.Int
reverseReplicationResponse_httpStatus = Lens.lens (\ReverseReplicationResponse' {httpStatus} -> httpStatus) (\s@ReverseReplicationResponse' {} a -> s {httpStatus = a} :: ReverseReplicationResponse)

instance Prelude.NFData ReverseReplicationResponse where
  rnf ReverseReplicationResponse' {..} =
    Prelude.rnf reversedDirectionSourceServerArn
      `Prelude.seq` Prelude.rnf httpStatus
