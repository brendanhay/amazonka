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
-- Module      : Network.AWS.IoT.DescribeAuditFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single audit finding. Properties include the
-- reason for noncompliance, the severity of the issue, and the start time
-- when the audit that returned the finding.
module Network.AWS.IoT.DescribeAuditFinding
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditFindingResponse'
            Prelude.<$> (x Core..?> "finding")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAuditFinding

instance Prelude.NFData DescribeAuditFinding

instance Core.ToHeaders DescribeAuditFinding where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAuditFinding where
  toPath DescribeAuditFinding' {..} =
    Prelude.mconcat
      ["/audit/findings/", Core.toBS findingId]

instance Core.ToQuery DescribeAuditFinding where
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

instance Prelude.NFData DescribeAuditFindingResponse
