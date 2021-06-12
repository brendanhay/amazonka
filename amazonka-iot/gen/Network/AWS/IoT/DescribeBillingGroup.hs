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
-- Module      : Network.AWS.IoT.DescribeBillingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a billing group.
module Network.AWS.IoT.DescribeBillingGroup
  ( -- * Creating a Request
    DescribeBillingGroup (..),
    newDescribeBillingGroup,

    -- * Request Lenses
    describeBillingGroup_billingGroupName,

    -- * Destructuring the Response
    DescribeBillingGroupResponse (..),
    newDescribeBillingGroupResponse,

    -- * Response Lenses
    describeBillingGroupResponse_billingGroupProperties,
    describeBillingGroupResponse_version,
    describeBillingGroupResponse_billingGroupArn,
    describeBillingGroupResponse_billingGroupId,
    describeBillingGroupResponse_billingGroupMetadata,
    describeBillingGroupResponse_billingGroupName,
    describeBillingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBillingGroup' smart constructor.
data DescribeBillingGroup = DescribeBillingGroup'
  { -- | The name of the billing group.
    billingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupName', 'describeBillingGroup_billingGroupName' - The name of the billing group.
newDescribeBillingGroup ::
  -- | 'billingGroupName'
  Core.Text ->
  DescribeBillingGroup
newDescribeBillingGroup pBillingGroupName_ =
  DescribeBillingGroup'
    { billingGroupName =
        pBillingGroupName_
    }

-- | The name of the billing group.
describeBillingGroup_billingGroupName :: Lens.Lens' DescribeBillingGroup Core.Text
describeBillingGroup_billingGroupName = Lens.lens (\DescribeBillingGroup' {billingGroupName} -> billingGroupName) (\s@DescribeBillingGroup' {} a -> s {billingGroupName = a} :: DescribeBillingGroup)

instance Core.AWSRequest DescribeBillingGroup where
  type
    AWSResponse DescribeBillingGroup =
      DescribeBillingGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBillingGroupResponse'
            Core.<$> (x Core..?> "billingGroupProperties")
            Core.<*> (x Core..?> "version")
            Core.<*> (x Core..?> "billingGroupArn")
            Core.<*> (x Core..?> "billingGroupId")
            Core.<*> (x Core..?> "billingGroupMetadata")
            Core.<*> (x Core..?> "billingGroupName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeBillingGroup

instance Core.NFData DescribeBillingGroup

instance Core.ToHeaders DescribeBillingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeBillingGroup where
  toPath DescribeBillingGroup' {..} =
    Core.mconcat
      ["/billing-groups/", Core.toBS billingGroupName]

instance Core.ToQuery DescribeBillingGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeBillingGroupResponse' smart constructor.
data DescribeBillingGroupResponse = DescribeBillingGroupResponse'
  { -- | The properties of the billing group.
    billingGroupProperties :: Core.Maybe BillingGroupProperties,
    -- | The version of the billing group.
    version :: Core.Maybe Core.Integer,
    -- | The ARN of the billing group.
    billingGroupArn :: Core.Maybe Core.Text,
    -- | The ID of the billing group.
    billingGroupId :: Core.Maybe Core.Text,
    -- | Additional information about the billing group.
    billingGroupMetadata :: Core.Maybe BillingGroupMetadata,
    -- | The name of the billing group.
    billingGroupName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupProperties', 'describeBillingGroupResponse_billingGroupProperties' - The properties of the billing group.
--
-- 'version', 'describeBillingGroupResponse_version' - The version of the billing group.
--
-- 'billingGroupArn', 'describeBillingGroupResponse_billingGroupArn' - The ARN of the billing group.
--
-- 'billingGroupId', 'describeBillingGroupResponse_billingGroupId' - The ID of the billing group.
--
-- 'billingGroupMetadata', 'describeBillingGroupResponse_billingGroupMetadata' - Additional information about the billing group.
--
-- 'billingGroupName', 'describeBillingGroupResponse_billingGroupName' - The name of the billing group.
--
-- 'httpStatus', 'describeBillingGroupResponse_httpStatus' - The response's http status code.
newDescribeBillingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeBillingGroupResponse
newDescribeBillingGroupResponse pHttpStatus_ =
  DescribeBillingGroupResponse'
    { billingGroupProperties =
        Core.Nothing,
      version = Core.Nothing,
      billingGroupArn = Core.Nothing,
      billingGroupId = Core.Nothing,
      billingGroupMetadata = Core.Nothing,
      billingGroupName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the billing group.
describeBillingGroupResponse_billingGroupProperties :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe BillingGroupProperties)
describeBillingGroupResponse_billingGroupProperties = Lens.lens (\DescribeBillingGroupResponse' {billingGroupProperties} -> billingGroupProperties) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupProperties = a} :: DescribeBillingGroupResponse)

-- | The version of the billing group.
describeBillingGroupResponse_version :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Core.Integer)
describeBillingGroupResponse_version = Lens.lens (\DescribeBillingGroupResponse' {version} -> version) (\s@DescribeBillingGroupResponse' {} a -> s {version = a} :: DescribeBillingGroupResponse)

-- | The ARN of the billing group.
describeBillingGroupResponse_billingGroupArn :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Core.Text)
describeBillingGroupResponse_billingGroupArn = Lens.lens (\DescribeBillingGroupResponse' {billingGroupArn} -> billingGroupArn) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupArn = a} :: DescribeBillingGroupResponse)

-- | The ID of the billing group.
describeBillingGroupResponse_billingGroupId :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Core.Text)
describeBillingGroupResponse_billingGroupId = Lens.lens (\DescribeBillingGroupResponse' {billingGroupId} -> billingGroupId) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupId = a} :: DescribeBillingGroupResponse)

-- | Additional information about the billing group.
describeBillingGroupResponse_billingGroupMetadata :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe BillingGroupMetadata)
describeBillingGroupResponse_billingGroupMetadata = Lens.lens (\DescribeBillingGroupResponse' {billingGroupMetadata} -> billingGroupMetadata) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupMetadata = a} :: DescribeBillingGroupResponse)

-- | The name of the billing group.
describeBillingGroupResponse_billingGroupName :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Core.Text)
describeBillingGroupResponse_billingGroupName = Lens.lens (\DescribeBillingGroupResponse' {billingGroupName} -> billingGroupName) (\s@DescribeBillingGroupResponse' {} a -> s {billingGroupName = a} :: DescribeBillingGroupResponse)

-- | The response's http status code.
describeBillingGroupResponse_httpStatus :: Lens.Lens' DescribeBillingGroupResponse Core.Int
describeBillingGroupResponse_httpStatus = Lens.lens (\DescribeBillingGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeBillingGroupResponse' {} a -> s {httpStatus = a} :: DescribeBillingGroupResponse)

instance Core.NFData DescribeBillingGroupResponse
