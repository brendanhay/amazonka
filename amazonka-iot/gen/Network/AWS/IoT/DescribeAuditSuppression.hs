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
-- Module      : Network.AWS.IoT.DescribeAuditSuppression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit suppression.
module Network.AWS.IoT.DescribeAuditSuppression
  ( -- * Creating a Request
    DescribeAuditSuppression (..),
    newDescribeAuditSuppression,

    -- * Request Lenses
    describeAuditSuppression_checkName,
    describeAuditSuppression_resourceIdentifier,

    -- * Destructuring the Response
    DescribeAuditSuppressionResponse (..),
    newDescribeAuditSuppressionResponse,

    -- * Response Lenses
    describeAuditSuppressionResponse_expirationDate,
    describeAuditSuppressionResponse_resourceIdentifier,
    describeAuditSuppressionResponse_checkName,
    describeAuditSuppressionResponse_description,
    describeAuditSuppressionResponse_suppressIndefinitely,
    describeAuditSuppressionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAuditSuppression' smart constructor.
data DescribeAuditSuppression = DescribeAuditSuppression'
  { checkName :: Core.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkName', 'describeAuditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'describeAuditSuppression_resourceIdentifier' - Undocumented member.
newDescribeAuditSuppression ::
  -- | 'checkName'
  Core.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  DescribeAuditSuppression
newDescribeAuditSuppression
  pCheckName_
  pResourceIdentifier_ =
    DescribeAuditSuppression'
      { checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | Undocumented member.
describeAuditSuppression_checkName :: Lens.Lens' DescribeAuditSuppression Core.Text
describeAuditSuppression_checkName = Lens.lens (\DescribeAuditSuppression' {checkName} -> checkName) (\s@DescribeAuditSuppression' {} a -> s {checkName = a} :: DescribeAuditSuppression)

-- | Undocumented member.
describeAuditSuppression_resourceIdentifier :: Lens.Lens' DescribeAuditSuppression ResourceIdentifier
describeAuditSuppression_resourceIdentifier = Lens.lens (\DescribeAuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@DescribeAuditSuppression' {} a -> s {resourceIdentifier = a} :: DescribeAuditSuppression)

instance Core.AWSRequest DescribeAuditSuppression where
  type
    AWSResponse DescribeAuditSuppression =
      DescribeAuditSuppressionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditSuppressionResponse'
            Core.<$> (x Core..?> "expirationDate")
            Core.<*> (x Core..?> "resourceIdentifier")
            Core.<*> (x Core..?> "checkName")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "suppressIndefinitely")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAuditSuppression

instance Core.NFData DescribeAuditSuppression

instance Core.ToHeaders DescribeAuditSuppression where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DescribeAuditSuppression where
  toJSON DescribeAuditSuppression' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("checkName" Core..= checkName),
            Core.Just
              ("resourceIdentifier" Core..= resourceIdentifier)
          ]
      )

instance Core.ToPath DescribeAuditSuppression where
  toPath = Core.const "/audit/suppressions/describe"

instance Core.ToQuery DescribeAuditSuppression where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAuditSuppressionResponse' smart constructor.
data DescribeAuditSuppressionResponse = DescribeAuditSuppressionResponse'
  { -- | The epoch timestamp in seconds at which this suppression expires.
    expirationDate :: Core.Maybe Core.POSIX,
    resourceIdentifier :: Core.Maybe ResourceIdentifier,
    checkName :: Core.Maybe Core.Text,
    -- | The description of the audit suppression.
    description :: Core.Maybe Core.Text,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAuditSuppressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDate', 'describeAuditSuppressionResponse_expirationDate' - The epoch timestamp in seconds at which this suppression expires.
--
-- 'resourceIdentifier', 'describeAuditSuppressionResponse_resourceIdentifier' - Undocumented member.
--
-- 'checkName', 'describeAuditSuppressionResponse_checkName' - Undocumented member.
--
-- 'description', 'describeAuditSuppressionResponse_description' - The description of the audit suppression.
--
-- 'suppressIndefinitely', 'describeAuditSuppressionResponse_suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- 'httpStatus', 'describeAuditSuppressionResponse_httpStatus' - The response's http status code.
newDescribeAuditSuppressionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAuditSuppressionResponse
newDescribeAuditSuppressionResponse pHttpStatus_ =
  DescribeAuditSuppressionResponse'
    { expirationDate =
        Core.Nothing,
      resourceIdentifier = Core.Nothing,
      checkName = Core.Nothing,
      description = Core.Nothing,
      suppressIndefinitely = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The epoch timestamp in seconds at which this suppression expires.
describeAuditSuppressionResponse_expirationDate :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Core.UTCTime)
describeAuditSuppressionResponse_expirationDate = Lens.lens (\DescribeAuditSuppressionResponse' {expirationDate} -> expirationDate) (\s@DescribeAuditSuppressionResponse' {} a -> s {expirationDate = a} :: DescribeAuditSuppressionResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
describeAuditSuppressionResponse_resourceIdentifier :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe ResourceIdentifier)
describeAuditSuppressionResponse_resourceIdentifier = Lens.lens (\DescribeAuditSuppressionResponse' {resourceIdentifier} -> resourceIdentifier) (\s@DescribeAuditSuppressionResponse' {} a -> s {resourceIdentifier = a} :: DescribeAuditSuppressionResponse)

-- | Undocumented member.
describeAuditSuppressionResponse_checkName :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Core.Text)
describeAuditSuppressionResponse_checkName = Lens.lens (\DescribeAuditSuppressionResponse' {checkName} -> checkName) (\s@DescribeAuditSuppressionResponse' {} a -> s {checkName = a} :: DescribeAuditSuppressionResponse)

-- | The description of the audit suppression.
describeAuditSuppressionResponse_description :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Core.Text)
describeAuditSuppressionResponse_description = Lens.lens (\DescribeAuditSuppressionResponse' {description} -> description) (\s@DescribeAuditSuppressionResponse' {} a -> s {description = a} :: DescribeAuditSuppressionResponse)

-- | Indicates whether a suppression should exist indefinitely or not.
describeAuditSuppressionResponse_suppressIndefinitely :: Lens.Lens' DescribeAuditSuppressionResponse (Core.Maybe Core.Bool)
describeAuditSuppressionResponse_suppressIndefinitely = Lens.lens (\DescribeAuditSuppressionResponse' {suppressIndefinitely} -> suppressIndefinitely) (\s@DescribeAuditSuppressionResponse' {} a -> s {suppressIndefinitely = a} :: DescribeAuditSuppressionResponse)

-- | The response's http status code.
describeAuditSuppressionResponse_httpStatus :: Lens.Lens' DescribeAuditSuppressionResponse Core.Int
describeAuditSuppressionResponse_httpStatus = Lens.lens (\DescribeAuditSuppressionResponse' {httpStatus} -> httpStatus) (\s@DescribeAuditSuppressionResponse' {} a -> s {httpStatus = a} :: DescribeAuditSuppressionResponse)

instance Core.NFData DescribeAuditSuppressionResponse
