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
-- Module      : Amazonka.CloudWatchLogs.PutRetentionPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the retention of the specified log group. With a retention policy,
-- you can configure the number of days for which to retain log events in
-- the specified log group.
--
-- CloudWatch Logs doesn’t immediately delete log events when they reach
-- their retention setting. It typically takes up to 72 hours after that
-- before log events are deleted, but in rare situations might take longer.
--
-- To illustrate, imagine that you change a log group to have a longer
-- retention setting when it contains log events that are past the
-- expiration date, but haven’t been deleted. Those log events will take up
-- to 72 hours to be deleted after the new retention date is reached. To
-- make sure that log data is deleted permanently, keep a log group at its
-- lower retention setting until 72 hours after the previous retention
-- period ends. Alternatively, wait to change the retention setting until
-- you confirm that the earlier log events are deleted.
module Amazonka.CloudWatchLogs.PutRetentionPolicy
  ( -- * Creating a Request
    PutRetentionPolicy (..),
    newPutRetentionPolicy,

    -- * Request Lenses
    putRetentionPolicy_logGroupName,
    putRetentionPolicy_retentionInDays,

    -- * Destructuring the Response
    PutRetentionPolicyResponse (..),
    newPutRetentionPolicyResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    retentionInDays :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'putRetentionPolicy_logGroupName' - The name of the log group.
--
-- 'retentionInDays', 'putRetentionPolicy_retentionInDays' - Undocumented member.
newPutRetentionPolicy ::
  -- | 'logGroupName'
  Prelude.Text ->
  -- | 'retentionInDays'
  Prelude.Int ->
  PutRetentionPolicy
newPutRetentionPolicy
  pLogGroupName_
  pRetentionInDays_ =
    PutRetentionPolicy'
      { logGroupName = pLogGroupName_,
        retentionInDays = pRetentionInDays_
      }

-- | The name of the log group.
putRetentionPolicy_logGroupName :: Lens.Lens' PutRetentionPolicy Prelude.Text
putRetentionPolicy_logGroupName = Lens.lens (\PutRetentionPolicy' {logGroupName} -> logGroupName) (\s@PutRetentionPolicy' {} a -> s {logGroupName = a} :: PutRetentionPolicy)

-- | Undocumented member.
putRetentionPolicy_retentionInDays :: Lens.Lens' PutRetentionPolicy Prelude.Int
putRetentionPolicy_retentionInDays = Lens.lens (\PutRetentionPolicy' {retentionInDays} -> retentionInDays) (\s@PutRetentionPolicy' {} a -> s {retentionInDays = a} :: PutRetentionPolicy)

instance Core.AWSRequest PutRetentionPolicy where
  type
    AWSResponse PutRetentionPolicy =
      PutRetentionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutRetentionPolicyResponse'

instance Prelude.Hashable PutRetentionPolicy where
  hashWithSalt _salt PutRetentionPolicy' {..} =
    _salt `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` retentionInDays

instance Prelude.NFData PutRetentionPolicy where
  rnf PutRetentionPolicy' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf retentionInDays

instance Data.ToHeaders PutRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutRetentionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logGroupName" Data..= logGroupName),
            Prelude.Just
              ("retentionInDays" Data..= retentionInDays)
          ]
      )

instance Data.ToPath PutRetentionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRetentionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutRetentionPolicyResponse ::
  PutRetentionPolicyResponse
newPutRetentionPolicyResponse =
  PutRetentionPolicyResponse'

instance Prelude.NFData PutRetentionPolicyResponse where
  rnf _ = ()
