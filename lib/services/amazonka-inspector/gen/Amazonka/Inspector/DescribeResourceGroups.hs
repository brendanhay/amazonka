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
-- Module      : Amazonka.Inspector.DescribeResourceGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the resource groups that are specified by the ARNs of the
-- resource groups.
module Amazonka.Inspector.DescribeResourceGroups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeResourceGroups' smart constructor.
data DescribeResourceGroups = DescribeResourceGroups'
  { -- | The ARN that specifies the resource group that you want to describe.
    resourceGroupArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  DescribeResourceGroups
newDescribeResourceGroups pResourceGroupArns_ =
  DescribeResourceGroups'
    { resourceGroupArns =
        Lens.coerced Lens.# pResourceGroupArns_
    }

-- | The ARN that specifies the resource group that you want to describe.
describeResourceGroups_resourceGroupArns :: Lens.Lens' DescribeResourceGroups (Prelude.NonEmpty Prelude.Text)
describeResourceGroups_resourceGroupArns = Lens.lens (\DescribeResourceGroups' {resourceGroupArns} -> resourceGroupArns) (\s@DescribeResourceGroups' {} a -> s {resourceGroupArns = a} :: DescribeResourceGroups) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeResourceGroups where
  type
    AWSResponse DescribeResourceGroups =
      DescribeResourceGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "resourceGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeResourceGroups where
  hashWithSalt _salt DescribeResourceGroups' {..} =
    _salt `Prelude.hashWithSalt` resourceGroupArns

instance Prelude.NFData DescribeResourceGroups where
  rnf DescribeResourceGroups' {..} =
    Prelude.rnf resourceGroupArns

instance Core.ToHeaders DescribeResourceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeResourceGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeResourceGroups where
  toJSON DescribeResourceGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceGroupArns" Core..= resourceGroupArns)
          ]
      )

instance Core.ToPath DescribeResourceGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeResourceGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourceGroupsResponse' smart constructor.
data DescribeResourceGroupsResponse = DescribeResourceGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about a resource group.
    resourceGroups :: [ResourceGroup],
    -- | Resource group details that cannot be described. An error code is
    -- provided for each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeResourceGroupsResponse
newDescribeResourceGroupsResponse pHttpStatus_ =
  DescribeResourceGroupsResponse'
    { httpStatus =
        pHttpStatus_,
      resourceGroups = Prelude.mempty,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
describeResourceGroupsResponse_httpStatus :: Lens.Lens' DescribeResourceGroupsResponse Prelude.Int
describeResourceGroupsResponse_httpStatus = Lens.lens (\DescribeResourceGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceGroupsResponse' {} a -> s {httpStatus = a} :: DescribeResourceGroupsResponse)

-- | Information about a resource group.
describeResourceGroupsResponse_resourceGroups :: Lens.Lens' DescribeResourceGroupsResponse [ResourceGroup]
describeResourceGroupsResponse_resourceGroups = Lens.lens (\DescribeResourceGroupsResponse' {resourceGroups} -> resourceGroups) (\s@DescribeResourceGroupsResponse' {} a -> s {resourceGroups = a} :: DescribeResourceGroupsResponse) Prelude.. Lens.coerced

-- | Resource group details that cannot be described. An error code is
-- provided for each failed item.
describeResourceGroupsResponse_failedItems :: Lens.Lens' DescribeResourceGroupsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeResourceGroupsResponse_failedItems = Lens.lens (\DescribeResourceGroupsResponse' {failedItems} -> failedItems) (\s@DescribeResourceGroupsResponse' {} a -> s {failedItems = a} :: DescribeResourceGroupsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeResourceGroupsResponse
  where
  rnf DescribeResourceGroupsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceGroups
      `Prelude.seq` Prelude.rnf failedItems
