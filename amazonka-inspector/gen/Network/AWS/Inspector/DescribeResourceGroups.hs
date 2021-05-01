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

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourceGroups' smart constructor.
data DescribeResourceGroups = DescribeResourceGroups'
  { -- | The ARN that specifies the resource group that you want to describe.
    resourceGroupArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude._Coerce Lens.# pResourceGroupArns_
    }

-- | The ARN that specifies the resource group that you want to describe.
describeResourceGroups_resourceGroupArns :: Lens.Lens' DescribeResourceGroups (Prelude.NonEmpty Prelude.Text)
describeResourceGroups_resourceGroupArns = Lens.lens (\DescribeResourceGroups' {resourceGroupArns} -> resourceGroupArns) (\s@DescribeResourceGroups' {} a -> s {resourceGroupArns = a} :: DescribeResourceGroups) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DescribeResourceGroups where
  type
    Rs DescribeResourceGroups =
      DescribeResourceGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "resourceGroups"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "failedItems"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DescribeResourceGroups

instance Prelude.NFData DescribeResourceGroups

instance Prelude.ToHeaders DescribeResourceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.DescribeResourceGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeResourceGroups where
  toJSON DescribeResourceGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceGroupArns" Prelude..= resourceGroupArns)
          ]
      )

instance Prelude.ToPath DescribeResourceGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeResourceGroups where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeResourceGroupsResponse_resourceGroups = Lens.lens (\DescribeResourceGroupsResponse' {resourceGroups} -> resourceGroups) (\s@DescribeResourceGroupsResponse' {} a -> s {resourceGroups = a} :: DescribeResourceGroupsResponse) Prelude.. Prelude._Coerce

-- | Resource group details that cannot be described. An error code is
-- provided for each failed item.
describeResourceGroupsResponse_failedItems :: Lens.Lens' DescribeResourceGroupsResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeResourceGroupsResponse_failedItems = Lens.lens (\DescribeResourceGroupsResponse' {failedItems} -> failedItems) (\s@DescribeResourceGroupsResponse' {} a -> s {failedItems = a} :: DescribeResourceGroupsResponse) Prelude.. Prelude._Coerce

instance
  Prelude.NFData
    DescribeResourceGroupsResponse
