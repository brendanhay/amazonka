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
-- Module      : Amazonka.EKS.DeleteAddon
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EKS.DeleteAddon
  ( -- * Creating a Request
    DeleteAddon (..),
    newDeleteAddon,

    -- * Request Lenses
    deleteAddon_preserve,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAddon' smart constructor.
data DeleteAddon = DeleteAddon'
  { -- | Specifying this option preserves the add-on software on your cluster but
    -- Amazon EKS stops managing any settings for the add-on. If an IAM account
    -- is associated with the add-on, it isn\'t removed.
    preserve :: Prelude.Maybe Prelude.Bool,
    -- | The name of the cluster to delete the add-on from.
    clusterName :: Prelude.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAddon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preserve', 'deleteAddon_preserve' - Specifying this option preserves the add-on software on your cluster but
-- Amazon EKS stops managing any settings for the add-on. If an IAM account
-- is associated with the add-on, it isn\'t removed.
--
-- 'clusterName', 'deleteAddon_clusterName' - The name of the cluster to delete the add-on from.
--
-- 'addonName', 'deleteAddon_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
newDeleteAddon ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'addonName'
  Prelude.Text ->
  DeleteAddon
newDeleteAddon pClusterName_ pAddonName_ =
  DeleteAddon'
    { preserve = Prelude.Nothing,
      clusterName = pClusterName_,
      addonName = pAddonName_
    }

-- | Specifying this option preserves the add-on software on your cluster but
-- Amazon EKS stops managing any settings for the add-on. If an IAM account
-- is associated with the add-on, it isn\'t removed.
deleteAddon_preserve :: Lens.Lens' DeleteAddon (Prelude.Maybe Prelude.Bool)
deleteAddon_preserve = Lens.lens (\DeleteAddon' {preserve} -> preserve) (\s@DeleteAddon' {} a -> s {preserve = a} :: DeleteAddon)

-- | The name of the cluster to delete the add-on from.
deleteAddon_clusterName :: Lens.Lens' DeleteAddon Prelude.Text
deleteAddon_clusterName = Lens.lens (\DeleteAddon' {clusterName} -> clusterName) (\s@DeleteAddon' {} a -> s {clusterName = a} :: DeleteAddon)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
deleteAddon_addonName :: Lens.Lens' DeleteAddon Prelude.Text
deleteAddon_addonName = Lens.lens (\DeleteAddon' {addonName} -> addonName) (\s@DeleteAddon' {} a -> s {addonName = a} :: DeleteAddon)

instance Core.AWSRequest DeleteAddon where
  type AWSResponse DeleteAddon = DeleteAddonResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAddonResponse'
            Prelude.<$> (x Data..?> "addon")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAddon where
  hashWithSalt _salt DeleteAddon' {..} =
    _salt `Prelude.hashWithSalt` preserve
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` addonName

instance Prelude.NFData DeleteAddon where
  rnf DeleteAddon' {..} =
    Prelude.rnf preserve
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf addonName

instance Data.ToHeaders DeleteAddon where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAddon where
  toPath DeleteAddon' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/addons/",
        Data.toBS addonName
      ]

instance Data.ToQuery DeleteAddon where
  toQuery DeleteAddon' {..} =
    Prelude.mconcat ["preserve" Data.=: preserve]

-- | /See:/ 'newDeleteAddonResponse' smart constructor.
data DeleteAddonResponse = DeleteAddonResponse'
  { addon :: Prelude.Maybe Addon,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteAddonResponse
newDeleteAddonResponse pHttpStatus_ =
  DeleteAddonResponse'
    { addon = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteAddonResponse_addon :: Lens.Lens' DeleteAddonResponse (Prelude.Maybe Addon)
deleteAddonResponse_addon = Lens.lens (\DeleteAddonResponse' {addon} -> addon) (\s@DeleteAddonResponse' {} a -> s {addon = a} :: DeleteAddonResponse)

-- | The response's http status code.
deleteAddonResponse_httpStatus :: Lens.Lens' DeleteAddonResponse Prelude.Int
deleteAddonResponse_httpStatus = Lens.lens (\DeleteAddonResponse' {httpStatus} -> httpStatus) (\s@DeleteAddonResponse' {} a -> s {httpStatus = a} :: DeleteAddonResponse)

instance Prelude.NFData DeleteAddonResponse where
  rnf DeleteAddonResponse' {..} =
    Prelude.rnf addon
      `Prelude.seq` Prelude.rnf httpStatus
