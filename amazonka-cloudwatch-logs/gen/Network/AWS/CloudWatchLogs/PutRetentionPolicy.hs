{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    retentionInDays :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest PutRetentionPolicy where
  type
    Rs PutRetentionPolicy =
      PutRetentionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutRetentionPolicyResponse'

instance Prelude.Hashable PutRetentionPolicy

instance Prelude.NFData PutRetentionPolicy

instance Prelude.ToHeaders PutRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.PutRetentionPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("logGroupName" Prelude..= logGroupName),
            Prelude.Just
              ("retentionInDays" Prelude..= retentionInDays)
          ]
      )

instance Prelude.ToPath PutRetentionPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutRetentionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutRetentionPolicyResponse ::
  PutRetentionPolicyResponse
newPutRetentionPolicyResponse =
  PutRetentionPolicyResponse'

instance Prelude.NFData PutRetentionPolicyResponse
