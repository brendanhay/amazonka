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
-- Module      : Amazonka.IoT.DescribeAuditFinding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single audit finding. Properties include the
-- reason for noncompliance, the severity of the issue, and the start time
-- when the audit that returned the finding.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeAuditFinding>
-- action.
module Amazonka.IoT.DescribeAuditFinding
  ( -- * Creating a Request
    DescribeAuditFinding (..),
    newDescribeAuditFinding,

    -- * Request Lenses
    describeAuditFinding_findingId,

    -- * Destructuring the Response
    DescribeAuditFindingResponse (..),
    newDescribeAuditFindingResponse,

    -- * Response Lenses
    describeAuditFindingResponse_finding,
    describeAuditFindingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAuditFinding' smart constructor.
data DescribeAuditFinding = DescribeAuditFinding'
  { -- | A unique identifier for a single audit finding. You can use this
    -- identifier to apply mitigation actions to the finding.
    findingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingId', 'describeAuditFinding_findingId' - A unique identifier for a single audit finding. You can use this
-- identifier to apply mitigation actions to the finding.
newDescribeAuditFinding ::
  -- | 'findingId'
  Prelude.Text ->
  DescribeAuditFinding
newDescribeAuditFinding pFindingId_ =
  DescribeAuditFinding' {findingId = pFindingId_}

-- | A unique identifier for a single audit finding. You can use this
-- identifier to apply mitigation actions to the finding.
describeAuditFinding_findingId :: Lens.Lens' DescribeAuditFinding Prelude.Text
describeAuditFinding_findingId = Lens.lens (\DescribeAuditFinding' {findingId} -> findingId) (\s@DescribeAuditFinding' {} a -> s {findingId = a} :: DescribeAuditFinding)

instance Core.AWSRequest DescribeAuditFinding where
  type
    AWSResponse DescribeAuditFinding =
      DescribeAuditFindingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditFindingResponse'
            Prelude.<$> (x Data..?> "finding")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAuditFinding where
  hashWithSalt _salt DescribeAuditFinding' {..} =
    _salt `Prelude.hashWithSalt` findingId

instance Prelude.NFData DescribeAuditFinding where
  rnf DescribeAuditFinding' {..} = Prelude.rnf findingId

instance Data.ToHeaders DescribeAuditFinding where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAuditFinding where
  toPath DescribeAuditFinding' {..} =
    Prelude.mconcat
      ["/audit/findings/", Data.toBS findingId]

instance Data.ToQuery DescribeAuditFinding where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAuditFindingResponse' smart constructor.
data DescribeAuditFindingResponse = DescribeAuditFindingResponse'
  { finding :: Prelude.Maybe AuditFinding,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditFindingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finding', 'describeAuditFindingResponse_finding' - Undocumented member.
--
-- 'httpStatus', 'describeAuditFindingResponse_httpStatus' - The response's http status code.
newDescribeAuditFindingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAuditFindingResponse
newDescribeAuditFindingResponse pHttpStatus_ =
  DescribeAuditFindingResponse'
    { finding =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeAuditFindingResponse_finding :: Lens.Lens' DescribeAuditFindingResponse (Prelude.Maybe AuditFinding)
describeAuditFindingResponse_finding = Lens.lens (\DescribeAuditFindingResponse' {finding} -> finding) (\s@DescribeAuditFindingResponse' {} a -> s {finding = a} :: DescribeAuditFindingResponse)

-- | The response's http status code.
describeAuditFindingResponse_httpStatus :: Lens.Lens' DescribeAuditFindingResponse Prelude.Int
describeAuditFindingResponse_httpStatus = Lens.lens (\DescribeAuditFindingResponse' {httpStatus} -> httpStatus) (\s@DescribeAuditFindingResponse' {} a -> s {httpStatus = a} :: DescribeAuditFindingResponse)

instance Prelude.NFData DescribeAuditFindingResponse where
  rnf DescribeAuditFindingResponse' {..} =
    Prelude.rnf finding
      `Prelude.seq` Prelude.rnf httpStatus
