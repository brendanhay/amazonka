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
-- Module      : Network.AWS.Inspector.DescribeResourceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the resource groups that are specified by the ARNs of the
-- resource groups.
module Network.AWS.Inspector.DescribeResourceGroups
  ( -- * Creating a Request
    DescribeResourceGroups (..),
    newDescribeResourceGroups,

    -- * Request Lenses
    describeResourceGroups_resourceGroupArns,

    -- * Destructuring the Response
    DescribeResourceGroupsResponse (..),
    newDescribeResourceGroupsResponse,

    -- * Response Lenses
    describeResourceGroupsResponse_httpStatus,
    describeResourceGroupsResponse_resourceGroups,
    describeResourceGroupsResponse_failedItems,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourceGroups' smart constructor.
data DescribeResourceGroups = DescribeResourceGroups'
  { -- | The ARN that specifies the resource group that you want to describe.
    resourceGroupArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeResourceGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupArns', 'describeResourceGroups_resourceGroupArns' - The ARN that specifies the resource group that you want to describe.
newDescribeResourceGroups ::
  -- | 'resourceGroupArns'
  Core.NonEmpty Core.Text ->
  DescribeResourceGroups
newDescribeResourceGroups pResourceGroupArns_ =
  DescribeResourceGroups'
    { resourceGroupArns =
        Lens._Coerce Lens.# pResourceGroupArns_
    }

-- | The ARN that specifies the resource group that you want to describe.
describeResourceGroups_resourceGroupArns :: Lens.Lens' DescribeResourceGroups (Core.NonEmpty Core.Text)
describeResourceGroups_resourceGroupArns = Lens.lens (\DescribeResourceGroups' {resourceGroupArns} -> resourceGroupArns) (\s@DescribeResourceGroups' {} a -> s {resourceGroupArns = a} :: DescribeResourceGroups) Core.. Lens._Coerce

instance Core.AWSRequest DescribeResourceGroups where
  type
    AWSResponse DescribeResourceGroups =
      DescribeResourceGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "resourceGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failedItems" Core..!@ Core.mempty)
      )

instance Core.Hashable DescribeResourceGroups

instance Core.NFData DescribeResourceGroups

instance Core.ToHeaders DescribeResourceGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeResourceGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeResourceGroups where
  toJSON DescribeResourceGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("resourceGroupArns" Core..= resourceGroupArns)
          ]
      )

instance Core.ToPath DescribeResourceGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeResourceGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeResourceGroupsResponse' smart constructor.
data DescribeResourceGroupsResponse = DescribeResourceGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Information about a resource group.
    resourceGroups :: [ResourceGroup],
    -- | Resource group details that cannot be described. An error code is
    -- provided for each failed item.
    failedItems :: Core.HashMap Core.Text FailedItemDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeResourceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeResourceGroupsResponse_httpStatus' - The response's http status code.
--
-- 'resourceGroups', 'describeResourceGroupsResponse_resourceGroups' - Information about a resource group.
--
-- 'failedItems', 'describeResourceGroupsResponse_failedItems' - Resource group details that cannot be described. An error code is
-- provided for each failed item.
newDescribeResourceGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeResourceGroupsResponse
newDescribeResourceGroupsResponse pHttpStatus_ =
  DescribeResourceGroupsResponse'
    { httpStatus =
        pHttpStatus_,
      resourceGroups = Core.mempty,
      failedItems = Core.mempty
    }

-- | The response's http status code.
describeResourceGroupsResponse_httpStatus :: Lens.Lens' DescribeResourceGroupsResponse Core.Int
describeResourceGroupsResponse_httpStatus = Lens.lens (\DescribeResourceGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceGroupsResponse' {} a -> s {httpStatus = a} :: DescribeResourceGroupsResponse)

-- | Information about a resource group.
describeResourceGroupsResponse_resourceGroups :: Lens.Lens' DescribeResourceGroupsResponse [ResourceGroup]
describeResourceGroupsResponse_resourceGroups = Lens.lens (\DescribeResourceGroupsResponse' {resourceGroups} -> resourceGroups) (\s@DescribeResourceGroupsResponse' {} a -> s {resourceGroups = a} :: DescribeResourceGroupsResponse) Core.. Lens._Coerce

-- | Resource group details that cannot be described. An error code is
-- provided for each failed item.
describeResourceGroupsResponse_failedItems :: Lens.Lens' DescribeResourceGroupsResponse (Core.HashMap Core.Text FailedItemDetails)
describeResourceGroupsResponse_failedItems = Lens.lens (\DescribeResourceGroupsResponse' {failedItems} -> failedItems) (\s@DescribeResourceGroupsResponse' {} a -> s {failedItems = a} :: DescribeResourceGroupsResponse) Core.. Lens._Coerce

instance Core.NFData DescribeResourceGroupsResponse
