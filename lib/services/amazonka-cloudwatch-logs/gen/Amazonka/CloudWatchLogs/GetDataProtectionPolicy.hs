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
-- Module      : Amazonka.CloudWatchLogs.GetDataProtectionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a log group data protection policy.
module Amazonka.CloudWatchLogs.GetDataProtectionPolicy
  ( -- * Creating a Request
    GetDataProtectionPolicy (..),
    newGetDataProtectionPolicy,

    -- * Request Lenses
    getDataProtectionPolicy_logGroupIdentifier,

    -- * Destructuring the Response
    GetDataProtectionPolicyResponse (..),
    newGetDataProtectionPolicyResponse,

    -- * Response Lenses
    getDataProtectionPolicyResponse_lastUpdatedTime,
    getDataProtectionPolicyResponse_logGroupIdentifier,
    getDataProtectionPolicyResponse_policyDocument,
    getDataProtectionPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataProtectionPolicy' smart constructor.
data GetDataProtectionPolicy = GetDataProtectionPolicy'
  { -- | The name or ARN of the log group that contains the data protection
    -- policy that you want to see.
    logGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataProtectionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupIdentifier', 'getDataProtectionPolicy_logGroupIdentifier' - The name or ARN of the log group that contains the data protection
-- policy that you want to see.
newGetDataProtectionPolicy ::
  -- | 'logGroupIdentifier'
  Prelude.Text ->
  GetDataProtectionPolicy
newGetDataProtectionPolicy pLogGroupIdentifier_ =
  GetDataProtectionPolicy'
    { logGroupIdentifier =
        pLogGroupIdentifier_
    }

-- | The name or ARN of the log group that contains the data protection
-- policy that you want to see.
getDataProtectionPolicy_logGroupIdentifier :: Lens.Lens' GetDataProtectionPolicy Prelude.Text
getDataProtectionPolicy_logGroupIdentifier = Lens.lens (\GetDataProtectionPolicy' {logGroupIdentifier} -> logGroupIdentifier) (\s@GetDataProtectionPolicy' {} a -> s {logGroupIdentifier = a} :: GetDataProtectionPolicy)

instance Core.AWSRequest GetDataProtectionPolicy where
  type
    AWSResponse GetDataProtectionPolicy =
      GetDataProtectionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataProtectionPolicyResponse'
            Prelude.<$> (x Data..?> "lastUpdatedTime")
            Prelude.<*> (x Data..?> "logGroupIdentifier")
            Prelude.<*> (x Data..?> "policyDocument")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataProtectionPolicy where
  hashWithSalt _salt GetDataProtectionPolicy' {..} =
    _salt `Prelude.hashWithSalt` logGroupIdentifier

instance Prelude.NFData GetDataProtectionPolicy where
  rnf GetDataProtectionPolicy' {..} =
    Prelude.rnf logGroupIdentifier

instance Data.ToHeaders GetDataProtectionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.GetDataProtectionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataProtectionPolicy where
  toJSON GetDataProtectionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("logGroupIdentifier" Data..= logGroupIdentifier)
          ]
      )

instance Data.ToPath GetDataProtectionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDataProtectionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataProtectionPolicyResponse' smart constructor.
data GetDataProtectionPolicyResponse = GetDataProtectionPolicyResponse'
  { -- | The date and time that this policy was most recently updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Natural,
    -- | The log group name or ARN that you specified in your request.
    logGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The data protection policy document for this log group.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataProtectionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'getDataProtectionPolicyResponse_lastUpdatedTime' - The date and time that this policy was most recently updated.
--
-- 'logGroupIdentifier', 'getDataProtectionPolicyResponse_logGroupIdentifier' - The log group name or ARN that you specified in your request.
--
-- 'policyDocument', 'getDataProtectionPolicyResponse_policyDocument' - The data protection policy document for this log group.
--
-- 'httpStatus', 'getDataProtectionPolicyResponse_httpStatus' - The response's http status code.
newGetDataProtectionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataProtectionPolicyResponse
newGetDataProtectionPolicyResponse pHttpStatus_ =
  GetDataProtectionPolicyResponse'
    { lastUpdatedTime =
        Prelude.Nothing,
      logGroupIdentifier = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that this policy was most recently updated.
getDataProtectionPolicyResponse_lastUpdatedTime :: Lens.Lens' GetDataProtectionPolicyResponse (Prelude.Maybe Prelude.Natural)
getDataProtectionPolicyResponse_lastUpdatedTime = Lens.lens (\GetDataProtectionPolicyResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@GetDataProtectionPolicyResponse' {} a -> s {lastUpdatedTime = a} :: GetDataProtectionPolicyResponse)

-- | The log group name or ARN that you specified in your request.
getDataProtectionPolicyResponse_logGroupIdentifier :: Lens.Lens' GetDataProtectionPolicyResponse (Prelude.Maybe Prelude.Text)
getDataProtectionPolicyResponse_logGroupIdentifier = Lens.lens (\GetDataProtectionPolicyResponse' {logGroupIdentifier} -> logGroupIdentifier) (\s@GetDataProtectionPolicyResponse' {} a -> s {logGroupIdentifier = a} :: GetDataProtectionPolicyResponse)

-- | The data protection policy document for this log group.
getDataProtectionPolicyResponse_policyDocument :: Lens.Lens' GetDataProtectionPolicyResponse (Prelude.Maybe Prelude.Text)
getDataProtectionPolicyResponse_policyDocument = Lens.lens (\GetDataProtectionPolicyResponse' {policyDocument} -> policyDocument) (\s@GetDataProtectionPolicyResponse' {} a -> s {policyDocument = a} :: GetDataProtectionPolicyResponse)

-- | The response's http status code.
getDataProtectionPolicyResponse_httpStatus :: Lens.Lens' GetDataProtectionPolicyResponse Prelude.Int
getDataProtectionPolicyResponse_httpStatus = Lens.lens (\GetDataProtectionPolicyResponse' {httpStatus} -> httpStatus) (\s@GetDataProtectionPolicyResponse' {} a -> s {httpStatus = a} :: GetDataProtectionPolicyResponse)

instance
  Prelude.NFData
    GetDataProtectionPolicyResponse
  where
  rnf GetDataProtectionPolicyResponse' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf logGroupIdentifier
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf httpStatus
