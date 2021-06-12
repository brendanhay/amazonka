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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAuditFinding' smart constructor.
data DescribeAuditFinding = DescribeAuditFinding'
  { -- | A unique identifier for a single audit finding. You can use this
    -- identifier to apply mitigation actions to the finding.
    findingId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeAuditFinding
newDescribeAuditFinding pFindingId_ =
  DescribeAuditFinding' {findingId = pFindingId_}

-- | A unique identifier for a single audit finding. You can use this
-- identifier to apply mitigation actions to the finding.
describeAuditFinding_findingId :: Lens.Lens' DescribeAuditFinding Core.Text
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
            Core.<$> (x Core..?> "finding")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAuditFinding

instance Core.NFData DescribeAuditFinding

instance Core.ToHeaders DescribeAuditFinding where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAuditFinding where
  toPath DescribeAuditFinding' {..} =
    Core.mconcat
      ["/audit/findings/", Core.toBS findingId]

instance Core.ToQuery DescribeAuditFinding where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAuditFindingResponse' smart constructor.
data DescribeAuditFindingResponse = DescribeAuditFindingResponse'
  { finding :: Core.Maybe AuditFinding,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAuditFindingResponse
newDescribeAuditFindingResponse pHttpStatus_ =
  DescribeAuditFindingResponse'
    { finding =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeAuditFindingResponse_finding :: Lens.Lens' DescribeAuditFindingResponse (Core.Maybe AuditFinding)
describeAuditFindingResponse_finding = Lens.lens (\DescribeAuditFindingResponse' {finding} -> finding) (\s@DescribeAuditFindingResponse' {} a -> s {finding = a} :: DescribeAuditFindingResponse)

-- | The response's http status code.
describeAuditFindingResponse_httpStatus :: Lens.Lens' DescribeAuditFindingResponse Core.Int
describeAuditFindingResponse_httpStatus = Lens.lens (\DescribeAuditFindingResponse' {httpStatus} -> httpStatus) (\s@DescribeAuditFindingResponse' {} a -> s {httpStatus = a} :: DescribeAuditFindingResponse)

instance Core.NFData DescribeAuditFindingResponse
