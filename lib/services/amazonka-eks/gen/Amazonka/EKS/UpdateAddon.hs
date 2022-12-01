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
-- Module      : Amazonka.EKS.UpdateAddon
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EKS add-on.
module Amazonka.EKS.UpdateAddon
  ( -- * Creating a Request
    UpdateAddon (..),
    newUpdateAddon,

    -- * Request Lenses
    updateAddon_clientRequestToken,
    updateAddon_addonVersion,
    updateAddon_serviceAccountRoleArn,
    updateAddon_resolveConflicts,
    updateAddon_clusterName,
    updateAddon_addonName,

    -- * Destructuring the Response
    UpdateAddonResponse (..),
    newUpdateAddonResponse,

    -- * Response Lenses
    updateAddonResponse_update,
    updateAddonResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAddon' smart constructor.
data UpdateAddon = UpdateAddon'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the add-on. The version must match one of the versions
    -- returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
    -- .
    addonVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an existing IAM role to bind to the
    -- add-on\'s service account. The role must be assigned the IAM permissions
    -- required by the add-on. If you don\'t specify an existing IAM role, then
    -- the add-on uses the permissions assigned to the node IAM role. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html Amazon EKS node IAM role>
    -- in the /Amazon EKS User Guide/.
    --
    -- To specify an existing IAM role, you must have an IAM OpenID Connect
    -- (OIDC) provider created for your cluster. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/enable-iam-roles-for-service-accounts.html Enabling IAM roles for service accounts on your cluster>
    -- in the /Amazon EKS User Guide/.
    serviceAccountRoleArn :: Prelude.Maybe Prelude.Text,
    -- | How to resolve field value conflicts for an Amazon EKS add-on if you\'ve
    -- changed a value from the Amazon EKS default value. Conflicts are handled
    -- based on the option you choose:
    --
    -- -   __None__ – Amazon EKS doesn\'t change the value. The update might
    --     fail.
    --
    -- -   __Overwrite__ – Amazon EKS overwrites the changed value back to the
    --     Amazon EKS default value.
    --
    -- -   __Preserve__ – Amazon EKS preserves the value. If you choose this
    --     option, we recommend that you test any field and value changes on a
    --     non-production cluster before updating the add-on on your production
    --     cluster.
    resolveConflicts :: Prelude.Maybe ResolveConflicts,
    -- | The name of the cluster.
    clusterName :: Prelude.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAddon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateAddon_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'addonVersion', 'updateAddon_addonVersion' - The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
--
-- 'serviceAccountRoleArn', 'updateAddon_serviceAccountRoleArn' - The Amazon Resource Name (ARN) of an existing IAM role to bind to the
-- add-on\'s service account. The role must be assigned the IAM permissions
-- required by the add-on. If you don\'t specify an existing IAM role, then
-- the add-on uses the permissions assigned to the node IAM role. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html Amazon EKS node IAM role>
-- in the /Amazon EKS User Guide/.
--
-- To specify an existing IAM role, you must have an IAM OpenID Connect
-- (OIDC) provider created for your cluster. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/enable-iam-roles-for-service-accounts.html Enabling IAM roles for service accounts on your cluster>
-- in the /Amazon EKS User Guide/.
--
-- 'resolveConflicts', 'updateAddon_resolveConflicts' - How to resolve field value conflicts for an Amazon EKS add-on if you\'ve
-- changed a value from the Amazon EKS default value. Conflicts are handled
-- based on the option you choose:
--
-- -   __None__ – Amazon EKS doesn\'t change the value. The update might
--     fail.
--
-- -   __Overwrite__ – Amazon EKS overwrites the changed value back to the
--     Amazon EKS default value.
--
-- -   __Preserve__ – Amazon EKS preserves the value. If you choose this
--     option, we recommend that you test any field and value changes on a
--     non-production cluster before updating the add-on on your production
--     cluster.
--
-- 'clusterName', 'updateAddon_clusterName' - The name of the cluster.
--
-- 'addonName', 'updateAddon_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
newUpdateAddon ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'addonName'
  Prelude.Text ->
  UpdateAddon
newUpdateAddon pClusterName_ pAddonName_ =
  UpdateAddon'
    { clientRequestToken = Prelude.Nothing,
      addonVersion = Prelude.Nothing,
      serviceAccountRoleArn = Prelude.Nothing,
      resolveConflicts = Prelude.Nothing,
      clusterName = pClusterName_,
      addonName = pAddonName_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateAddon_clientRequestToken :: Lens.Lens' UpdateAddon (Prelude.Maybe Prelude.Text)
updateAddon_clientRequestToken = Lens.lens (\UpdateAddon' {clientRequestToken} -> clientRequestToken) (\s@UpdateAddon' {} a -> s {clientRequestToken = a} :: UpdateAddon)

-- | The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
updateAddon_addonVersion :: Lens.Lens' UpdateAddon (Prelude.Maybe Prelude.Text)
updateAddon_addonVersion = Lens.lens (\UpdateAddon' {addonVersion} -> addonVersion) (\s@UpdateAddon' {} a -> s {addonVersion = a} :: UpdateAddon)

-- | The Amazon Resource Name (ARN) of an existing IAM role to bind to the
-- add-on\'s service account. The role must be assigned the IAM permissions
-- required by the add-on. If you don\'t specify an existing IAM role, then
-- the add-on uses the permissions assigned to the node IAM role. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/create-node-role.html Amazon EKS node IAM role>
-- in the /Amazon EKS User Guide/.
--
-- To specify an existing IAM role, you must have an IAM OpenID Connect
-- (OIDC) provider created for your cluster. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/enable-iam-roles-for-service-accounts.html Enabling IAM roles for service accounts on your cluster>
-- in the /Amazon EKS User Guide/.
updateAddon_serviceAccountRoleArn :: Lens.Lens' UpdateAddon (Prelude.Maybe Prelude.Text)
updateAddon_serviceAccountRoleArn = Lens.lens (\UpdateAddon' {serviceAccountRoleArn} -> serviceAccountRoleArn) (\s@UpdateAddon' {} a -> s {serviceAccountRoleArn = a} :: UpdateAddon)

-- | How to resolve field value conflicts for an Amazon EKS add-on if you\'ve
-- changed a value from the Amazon EKS default value. Conflicts are handled
-- based on the option you choose:
--
-- -   __None__ – Amazon EKS doesn\'t change the value. The update might
--     fail.
--
-- -   __Overwrite__ – Amazon EKS overwrites the changed value back to the
--     Amazon EKS default value.
--
-- -   __Preserve__ – Amazon EKS preserves the value. If you choose this
--     option, we recommend that you test any field and value changes on a
--     non-production cluster before updating the add-on on your production
--     cluster.
updateAddon_resolveConflicts :: Lens.Lens' UpdateAddon (Prelude.Maybe ResolveConflicts)
updateAddon_resolveConflicts = Lens.lens (\UpdateAddon' {resolveConflicts} -> resolveConflicts) (\s@UpdateAddon' {} a -> s {resolveConflicts = a} :: UpdateAddon)

-- | The name of the cluster.
updateAddon_clusterName :: Lens.Lens' UpdateAddon Prelude.Text
updateAddon_clusterName = Lens.lens (\UpdateAddon' {clusterName} -> clusterName) (\s@UpdateAddon' {} a -> s {clusterName = a} :: UpdateAddon)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
updateAddon_addonName :: Lens.Lens' UpdateAddon Prelude.Text
updateAddon_addonName = Lens.lens (\UpdateAddon' {addonName} -> addonName) (\s@UpdateAddon' {} a -> s {addonName = a} :: UpdateAddon)

instance Core.AWSRequest UpdateAddon where
  type AWSResponse UpdateAddon = UpdateAddonResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAddonResponse'
            Prelude.<$> (x Core..?> "update")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAddon where
  hashWithSalt _salt UpdateAddon' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` addonVersion
      `Prelude.hashWithSalt` serviceAccountRoleArn
      `Prelude.hashWithSalt` resolveConflicts
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` addonName

instance Prelude.NFData UpdateAddon where
  rnf UpdateAddon' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf addonVersion
      `Prelude.seq` Prelude.rnf serviceAccountRoleArn
      `Prelude.seq` Prelude.rnf resolveConflicts
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf addonName

instance Core.ToHeaders UpdateAddon where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAddon where
  toJSON UpdateAddon' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("addonVersion" Core..=) Prelude.<$> addonVersion,
            ("serviceAccountRoleArn" Core..=)
              Prelude.<$> serviceAccountRoleArn,
            ("resolveConflicts" Core..=)
              Prelude.<$> resolveConflicts
          ]
      )

instance Core.ToPath UpdateAddon where
  toPath UpdateAddon' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/addons/",
        Core.toBS addonName,
        "/update"
      ]

instance Core.ToQuery UpdateAddon where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAddonResponse' smart constructor.
data UpdateAddonResponse = UpdateAddonResponse'
  { update :: Prelude.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAddonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'updateAddonResponse_update' - Undocumented member.
--
-- 'httpStatus', 'updateAddonResponse_httpStatus' - The response's http status code.
newUpdateAddonResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAddonResponse
newUpdateAddonResponse pHttpStatus_ =
  UpdateAddonResponse'
    { update = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateAddonResponse_update :: Lens.Lens' UpdateAddonResponse (Prelude.Maybe Update)
updateAddonResponse_update = Lens.lens (\UpdateAddonResponse' {update} -> update) (\s@UpdateAddonResponse' {} a -> s {update = a} :: UpdateAddonResponse)

-- | The response's http status code.
updateAddonResponse_httpStatus :: Lens.Lens' UpdateAddonResponse Prelude.Int
updateAddonResponse_httpStatus = Lens.lens (\UpdateAddonResponse' {httpStatus} -> httpStatus) (\s@UpdateAddonResponse' {} a -> s {httpStatus = a} :: UpdateAddonResponse)

instance Prelude.NFData UpdateAddonResponse where
  rnf UpdateAddonResponse' {..} =
    Prelude.rnf update
      `Prelude.seq` Prelude.rnf httpStatus
