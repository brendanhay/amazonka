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
-- Module      : Network.AWS.EKS.DescribeAddon
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon EKS add-on.
module Network.AWS.EKS.DescribeAddon
  ( -- * Creating a Request
    DescribeAddon (..),
    newDescribeAddon,

    -- * Request Lenses
    describeAddon_clusterName,
    describeAddon_addonName,

    -- * Destructuring the Response
    DescribeAddonResponse (..),
    newDescribeAddonResponse,

    -- * Response Lenses
    describeAddonResponse_addon,
    describeAddonResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAddon' smart constructor.
data DescribeAddon = DescribeAddon'
  { -- | The name of the cluster.
    clusterName :: Core.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'describeAddon_clusterName' - The name of the cluster.
--
-- 'addonName', 'describeAddon_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
newDescribeAddon ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'addonName'
  Core.Text ->
  DescribeAddon
newDescribeAddon pClusterName_ pAddonName_ =
  DescribeAddon'
    { clusterName = pClusterName_,
      addonName = pAddonName_
    }

-- | The name of the cluster.
describeAddon_clusterName :: Lens.Lens' DescribeAddon Core.Text
describeAddon_clusterName = Lens.lens (\DescribeAddon' {clusterName} -> clusterName) (\s@DescribeAddon' {} a -> s {clusterName = a} :: DescribeAddon)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
describeAddon_addonName :: Lens.Lens' DescribeAddon Core.Text
describeAddon_addonName = Lens.lens (\DescribeAddon' {addonName} -> addonName) (\s@DescribeAddon' {} a -> s {addonName = a} :: DescribeAddon)

instance Core.AWSRequest DescribeAddon where
  type
    AWSResponse DescribeAddon =
      DescribeAddonResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddonResponse'
            Core.<$> (x Core..?> "addon")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAddon

instance Core.NFData DescribeAddon

instance Core.ToHeaders DescribeAddon where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeAddon where
  toPath DescribeAddon' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/addons/",
        Core.toBS addonName
      ]

instance Core.ToQuery DescribeAddon where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAddonResponse' smart constructor.
data DescribeAddonResponse = DescribeAddonResponse'
  { addon :: Core.Maybe Addon,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAddonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addon', 'describeAddonResponse_addon' - Undocumented member.
--
-- 'httpStatus', 'describeAddonResponse_httpStatus' - The response's http status code.
newDescribeAddonResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAddonResponse
newDescribeAddonResponse pHttpStatus_ =
  DescribeAddonResponse'
    { addon = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeAddonResponse_addon :: Lens.Lens' DescribeAddonResponse (Core.Maybe Addon)
describeAddonResponse_addon = Lens.lens (\DescribeAddonResponse' {addon} -> addon) (\s@DescribeAddonResponse' {} a -> s {addon = a} :: DescribeAddonResponse)

-- | The response's http status code.
describeAddonResponse_httpStatus :: Lens.Lens' DescribeAddonResponse Core.Int
describeAddonResponse_httpStatus = Lens.lens (\DescribeAddonResponse' {httpStatus} -> httpStatus) (\s@DescribeAddonResponse' {} a -> s {httpStatus = a} :: DescribeAddonResponse)

instance Core.NFData DescribeAddonResponse
