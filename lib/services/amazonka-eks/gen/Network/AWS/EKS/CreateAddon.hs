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
-- Module      : Network.AWS.EKS.CreateAddon
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EKS add-on.
--
-- Amazon EKS add-ons help to automate the provisioning and lifecycle
-- management of common operational software for Amazon EKS clusters.
-- Amazon EKS add-ons can only be used with Amazon EKS clusters running
-- version 1.18 with platform version @eks.3@ or later because add-ons rely
-- on the Server-side Apply Kubernetes feature, which is only available in
-- Kubernetes 1.18 and later.
module Network.AWS.EKS.CreateAddon
  ( -- * Creating a Request
    CreateAddon (..),
    newCreateAddon,

    -- * Request Lenses
    createAddon_addonVersion,
    createAddon_serviceAccountRoleArn,
    createAddon_resolveConflicts,
    createAddon_clientRequestToken,
    createAddon_tags,
    createAddon_clusterName,
    createAddon_addonName,

    -- * Destructuring the Response
    CreateAddonResponse (..),
    newCreateAddonResponse,

    -- * Response Lenses
    createAddonResponse_addon,
    createAddonResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAddon' smart constructor.
data CreateAddon = CreateAddon'
  { -- | The version of the add-on. The version must match one of the versions
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
    -- | How to resolve parameter value conflicts when migrating an existing
    -- add-on to an Amazon EKS add-on.
    resolveConflicts :: Prelude.Maybe ResolveConflicts,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The metadata to apply to the cluster to assist with categorization and
    -- organization. Each tag consists of a key and an optional value, both of
    -- which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the cluster to create the add-on for.
    clusterName :: Prelude.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
    -- .
    addonName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAddon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonVersion', 'createAddon_addonVersion' - The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
--
-- 'serviceAccountRoleArn', 'createAddon_serviceAccountRoleArn' - The Amazon Resource Name (ARN) of an existing IAM role to bind to the
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
-- 'resolveConflicts', 'createAddon_resolveConflicts' - How to resolve parameter value conflicts when migrating an existing
-- add-on to an Amazon EKS add-on.
--
-- 'clientRequestToken', 'createAddon_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'tags', 'createAddon_tags' - The metadata to apply to the cluster to assist with categorization and
-- organization. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- 'clusterName', 'createAddon_clusterName' - The name of the cluster to create the add-on for.
--
-- 'addonName', 'createAddon_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
newCreateAddon ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'addonName'
  Prelude.Text ->
  CreateAddon
newCreateAddon pClusterName_ pAddonName_ =
  CreateAddon'
    { addonVersion = Prelude.Nothing,
      serviceAccountRoleArn = Prelude.Nothing,
      resolveConflicts = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      clusterName = pClusterName_,
      addonName = pAddonName_
    }

-- | The version of the add-on. The version must match one of the versions
-- returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
createAddon_addonVersion :: Lens.Lens' CreateAddon (Prelude.Maybe Prelude.Text)
createAddon_addonVersion = Lens.lens (\CreateAddon' {addonVersion} -> addonVersion) (\s@CreateAddon' {} a -> s {addonVersion = a} :: CreateAddon)

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
createAddon_serviceAccountRoleArn :: Lens.Lens' CreateAddon (Prelude.Maybe Prelude.Text)
createAddon_serviceAccountRoleArn = Lens.lens (\CreateAddon' {serviceAccountRoleArn} -> serviceAccountRoleArn) (\s@CreateAddon' {} a -> s {serviceAccountRoleArn = a} :: CreateAddon)

-- | How to resolve parameter value conflicts when migrating an existing
-- add-on to an Amazon EKS add-on.
createAddon_resolveConflicts :: Lens.Lens' CreateAddon (Prelude.Maybe ResolveConflicts)
createAddon_resolveConflicts = Lens.lens (\CreateAddon' {resolveConflicts} -> resolveConflicts) (\s@CreateAddon' {} a -> s {resolveConflicts = a} :: CreateAddon)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createAddon_clientRequestToken :: Lens.Lens' CreateAddon (Prelude.Maybe Prelude.Text)
createAddon_clientRequestToken = Lens.lens (\CreateAddon' {clientRequestToken} -> clientRequestToken) (\s@CreateAddon' {} a -> s {clientRequestToken = a} :: CreateAddon)

-- | The metadata to apply to the cluster to assist with categorization and
-- organization. Each tag consists of a key and an optional value, both of
-- which you define.
createAddon_tags :: Lens.Lens' CreateAddon (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAddon_tags = Lens.lens (\CreateAddon' {tags} -> tags) (\s@CreateAddon' {} a -> s {tags = a} :: CreateAddon) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cluster to create the add-on for.
createAddon_clusterName :: Lens.Lens' CreateAddon Prelude.Text
createAddon_clusterName = Lens.lens (\CreateAddon' {clusterName} -> clusterName) (\s@CreateAddon' {} a -> s {clusterName = a} :: CreateAddon)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
-- .
createAddon_addonName :: Lens.Lens' CreateAddon Prelude.Text
createAddon_addonName = Lens.lens (\CreateAddon' {addonName} -> addonName) (\s@CreateAddon' {} a -> s {addonName = a} :: CreateAddon)

instance Core.AWSRequest CreateAddon where
  type AWSResponse CreateAddon = CreateAddonResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAddonResponse'
            Prelude.<$> (x Core..?> "addon")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAddon

instance Prelude.NFData CreateAddon

instance Core.ToHeaders CreateAddon where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAddon where
  toJSON CreateAddon' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("addonVersion" Core..=) Prelude.<$> addonVersion,
            ("serviceAccountRoleArn" Core..=)
              Prelude.<$> serviceAccountRoleArn,
            ("resolveConflicts" Core..=)
              Prelude.<$> resolveConflicts,
            ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("addonName" Core..= addonName)
          ]
      )

instance Core.ToPath CreateAddon where
  toPath CreateAddon' {..} =
    Prelude.mconcat
      ["/clusters/", Core.toBS clusterName, "/addons"]

instance Core.ToQuery CreateAddon where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAddonResponse' smart constructor.
data CreateAddonResponse = CreateAddonResponse'
  { addon :: Prelude.Maybe Addon,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAddonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addon', 'createAddonResponse_addon' - Undocumented member.
--
-- 'httpStatus', 'createAddonResponse_httpStatus' - The response's http status code.
newCreateAddonResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAddonResponse
newCreateAddonResponse pHttpStatus_ =
  CreateAddonResponse'
    { addon = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createAddonResponse_addon :: Lens.Lens' CreateAddonResponse (Prelude.Maybe Addon)
createAddonResponse_addon = Lens.lens (\CreateAddonResponse' {addon} -> addon) (\s@CreateAddonResponse' {} a -> s {addon = a} :: CreateAddonResponse)

-- | The response's http status code.
createAddonResponse_httpStatus :: Lens.Lens' CreateAddonResponse Prelude.Int
createAddonResponse_httpStatus = Lens.lens (\CreateAddonResponse' {httpStatus} -> httpStatus) (\s@CreateAddonResponse' {} a -> s {httpStatus = a} :: CreateAddonResponse)

instance Prelude.NFData CreateAddonResponse
