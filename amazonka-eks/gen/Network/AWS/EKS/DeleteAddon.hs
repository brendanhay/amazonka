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
-- Module      : Network.AWS.EKS.DeleteAddon
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an Amazon EKS add-on.
--
-- When you remove the add-on, it will also be deleted from the cluster.
-- You can always manually start an add-on on the cluster using the
-- Kubernetes API.
module Network.AWS.EKS.DeleteAddon
  ( -- * Creating a Request
    DeleteAddon (..),
    newDeleteAddon,

    -- * Request Lenses
    deleteAddon_clusterName,
    deleteAddon_addonName,

    -- * Destructuring the Response
    DeleteAddonResponse (..),
    newDeleteAddonResponse,

    -- * Response Lenses
    deleteAddonResponse_addon,
    deleteAddonResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAddon' smart constructor.
data DeleteAddon = DeleteAddon'
  { -- | The name of the cluster to delete the add-on from.
    clusterName :: Core.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAddon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'deleteAddon_clusterName' - The name of the cluster to delete the add-on from.
--
-- 'addonName', 'deleteAddon_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
newDeleteAddon ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'addonName'
  Core.Text ->
  DeleteAddon
newDeleteAddon pClusterName_ pAddonName_ =
  DeleteAddon'
    { clusterName = pClusterName_,
      addonName = pAddonName_
    }

-- | The name of the cluster to delete the add-on from.
deleteAddon_clusterName :: Lens.Lens' DeleteAddon Core.Text
deleteAddon_clusterName = Lens.lens (\DeleteAddon' {clusterName} -> clusterName) (\s@DeleteAddon' {} a -> s {clusterName = a} :: DeleteAddon)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
deleteAddon_addonName :: Lens.Lens' DeleteAddon Core.Text
deleteAddon_addonName = Lens.lens (\DeleteAddon' {addonName} -> addonName) (\s@DeleteAddon' {} a -> s {addonName = a} :: DeleteAddon)

instance Core.AWSRequest DeleteAddon where
  type AWSResponse DeleteAddon = DeleteAddonResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAddonResponse'
            Core.<$> (x Core..?> "addon")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAddon

instance Core.NFData DeleteAddon

instance Core.ToHeaders DeleteAddon where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteAddon where
  toPath DeleteAddon' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/addons/",
        Core.toBS addonName
      ]

instance Core.ToQuery DeleteAddon where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAddonResponse' smart constructor.
data DeleteAddonResponse = DeleteAddonResponse'
  { addon :: Core.Maybe Addon,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAddonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addon', 'deleteAddonResponse_addon' - Undocumented member.
--
-- 'httpStatus', 'deleteAddonResponse_httpStatus' - The response's http status code.
newDeleteAddonResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAddonResponse
newDeleteAddonResponse pHttpStatus_ =
  DeleteAddonResponse'
    { addon = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteAddonResponse_addon :: Lens.Lens' DeleteAddonResponse (Core.Maybe Addon)
deleteAddonResponse_addon = Lens.lens (\DeleteAddonResponse' {addon} -> addon) (\s@DeleteAddonResponse' {} a -> s {addon = a} :: DeleteAddonResponse)

-- | The response's http status code.
deleteAddonResponse_httpStatus :: Lens.Lens' DeleteAddonResponse Core.Int
deleteAddonResponse_httpStatus = Lens.lens (\DeleteAddonResponse' {httpStatus} -> httpStatus) (\s@DeleteAddonResponse' {} a -> s {httpStatus = a} :: DeleteAddonResponse)

instance Core.NFData DeleteAddonResponse
