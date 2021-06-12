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
-- Module      : Network.AWS.CloudWatchLogs.PutRetentionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the retention of the specified log group. A retention policy allows
-- you to configure the number of days for which to retain log events in
-- the specified log group.
module Network.AWS.CloudWatchLogs.PutRetentionPolicy
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

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    retentionInDays :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'retentionInDays'
  Core.Int ->
  PutRetentionPolicy
newPutRetentionPolicy
  pLogGroupName_
  pRetentionInDays_ =
    PutRetentionPolicy'
      { logGroupName = pLogGroupName_,
        retentionInDays = pRetentionInDays_
      }

-- | The name of the log group.
putRetentionPolicy_logGroupName :: Lens.Lens' PutRetentionPolicy Core.Text
putRetentionPolicy_logGroupName = Lens.lens (\PutRetentionPolicy' {logGroupName} -> logGroupName) (\s@PutRetentionPolicy' {} a -> s {logGroupName = a} :: PutRetentionPolicy)

-- | Undocumented member.
putRetentionPolicy_retentionInDays :: Lens.Lens' PutRetentionPolicy Core.Int
putRetentionPolicy_retentionInDays = Lens.lens (\PutRetentionPolicy' {retentionInDays} -> retentionInDays) (\s@PutRetentionPolicy' {} a -> s {retentionInDays = a} :: PutRetentionPolicy)

instance Core.AWSRequest PutRetentionPolicy where
  type
    AWSResponse PutRetentionPolicy =
      PutRetentionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutRetentionPolicyResponse'

instance Core.Hashable PutRetentionPolicy

instance Core.NFData PutRetentionPolicy

instance Core.ToHeaders PutRetentionPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.PutRetentionPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just
              ("retentionInDays" Core..= retentionInDays)
          ]
      )

instance Core.ToPath PutRetentionPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutRetentionPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutRetentionPolicyResponse ::
  PutRetentionPolicyResponse
newPutRetentionPolicyResponse =
  PutRetentionPolicyResponse'

instance Core.NFData PutRetentionPolicyResponse
