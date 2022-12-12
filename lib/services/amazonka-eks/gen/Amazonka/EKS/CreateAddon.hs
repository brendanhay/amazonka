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
-- Module      : Amazonka.EKS.CreateAddon
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EKS add-on.
--
-- Amazon EKS add-ons help to automate the provisioning and lifecycle
-- management of common operational software for Amazon EKS clusters. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-add-ons.html Amazon EKS add-ons>
-- in the /Amazon EKS User Guide/.
module Amazonka.EKS.CreateAddon
  ( -- * Creating a Request
    CreateAddon (..),
    newCreateAddon,

    -- * Request Lenses
    createAddon_addonVersion,
    createAddon_clientRequestToken,
    createAddon_configurationValues,
    createAddon_resolveConflicts,
    createAddon_serviceAccountRoleArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAddon' smart constructor.
data CreateAddon = CreateAddon'
  { -- | The version of the add-on. The version must match one of the versions
    -- returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonVersions.html DescribeAddonVersions>
    -- .
    addonVersion :: Prelude.Maybe Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The set of configuration values for the add-on being created. Whatever
    -- values provided here are validated against the schema from
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonConfiguration.html DescribeAddonConfiguration>
    -- .
    configurationValues :: Prelude.Maybe Prelude.Text,
    -- | How to resolve field value conflicts for an Amazon EKS add-on. Conflicts
    -- are handled based on the value you choose:
    --
    -- -   __None__ – If the self-managed version of the add-on is installed on
    --     your cluster, Amazon EKS doesn\'t change the value. Creation of the
    --     add-on might fail.
    --
    -- -   __Overwrite__ – If the self-managed version of the add-on is
    --     installed on your cluster and the Amazon EKS default value is
    --     different than the existing value, Amazon EKS changes the value to
    --     the Amazon EKS default value.
    --
    -- -   __Preserve__ – Not supported. You can set this value when updating
    --     an add-on though. For more information, see
    --     <https://docs.aws.amazon.com/eks/latest/APIReference/API_UpdateAddon.html UpdateAddon>.
    --
    -- If you don\'t currently have the self-managed version of the add-on
    -- installed on your cluster, the Amazon EKS add-on is installed. Amazon
    -- EKS sets all values to default values, regardless of the option that you
    -- specify.
    resolveConflicts :: Prelude.Maybe ResolveConflicts,
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
    -- | The metadata to apply to the cluster to assist with categorization and
    -- organization. Each tag consists of a key and an optional value. You
    -- define both.
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
-- 'clientRequestToken', 'createAddon_clientRequestToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'configurationValues', 'createAddon_configurationValues' - The set of configuration values for the add-on being created. Whatever
-- values provided here are validated against the schema from
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonConfiguration.html DescribeAddonConfiguration>
-- .
--
-- 'resolveConflicts', 'createAddon_resolveConflicts' - How to resolve field value conflicts for an Amazon EKS add-on. Conflicts
-- are handled based on the value you choose:
--
-- -   __None__ – If the self-managed version of the add-on is installed on
--     your cluster, Amazon EKS doesn\'t change the value. Creation of the
--     add-on might fail.
--
-- -   __Overwrite__ – If the self-managed version of the add-on is
--     installed on your cluster and the Amazon EKS default value is
--     different than the existing value, Amazon EKS changes the value to
--     the Amazon EKS default value.
--
-- -   __Preserve__ – Not supported. You can set this value when updating
--     an add-on though. For more information, see
--     <https://docs.aws.amazon.com/eks/latest/APIReference/API_UpdateAddon.html UpdateAddon>.
--
-- If you don\'t currently have the self-managed version of the add-on
-- installed on your cluster, the Amazon EKS add-on is installed. Amazon
-- EKS sets all values to default values, regardless of the option that you
-- specify.
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
-- 'tags', 'createAddon_tags' - The metadata to apply to the cluster to assist with categorization and
-- organization. Each tag consists of a key and an optional value. You
-- define both.
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
      clientRequestToken = Prelude.Nothing,
      configurationValues = Prelude.Nothing,
      resolveConflicts = Prelude.Nothing,
      serviceAccountRoleArn = Prelude.Nothing,
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

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createAddon_clientRequestToken :: Lens.Lens' CreateAddon (Prelude.Maybe Prelude.Text)
createAddon_clientRequestToken = Lens.lens (\CreateAddon' {clientRequestToken} -> clientRequestToken) (\s@CreateAddon' {} a -> s {clientRequestToken = a} :: CreateAddon)

-- | The set of configuration values for the add-on being created. Whatever
-- values provided here are validated against the schema from
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_DescribeAddonConfiguration.html DescribeAddonConfiguration>
-- .
createAddon_configurationValues :: Lens.Lens' CreateAddon (Prelude.Maybe Prelude.Text)
createAddon_configurationValues = Lens.lens (\CreateAddon' {configurationValues} -> configurationValues) (\s@CreateAddon' {} a -> s {configurationValues = a} :: CreateAddon)

-- | How to resolve field value conflicts for an Amazon EKS add-on. Conflicts
-- are handled based on the value you choose:
--
-- -   __None__ – If the self-managed version of the add-on is installed on
--     your cluster, Amazon EKS doesn\'t change the value. Creation of the
--     add-on might fail.
--
-- -   __Overwrite__ – If the self-managed version of the add-on is
--     installed on your cluster and the Amazon EKS default value is
--     different than the existing value, Amazon EKS changes the value to
--     the Amazon EKS default value.
--
-- -   __Preserve__ – Not supported. You can set this value when updating
--     an add-on though. For more information, see
--     <https://docs.aws.amazon.com/eks/latest/APIReference/API_UpdateAddon.html UpdateAddon>.
--
-- If you don\'t currently have the self-managed version of the add-on
-- installed on your cluster, the Amazon EKS add-on is installed. Amazon
-- EKS sets all values to default values, regardless of the option that you
-- specify.
createAddon_resolveConflicts :: Lens.Lens' CreateAddon (Prelude.Maybe ResolveConflicts)
createAddon_resolveConflicts = Lens.lens (\CreateAddon' {resolveConflicts} -> resolveConflicts) (\s@CreateAddon' {} a -> s {resolveConflicts = a} :: CreateAddon)

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

-- | The metadata to apply to the cluster to assist with categorization and
-- organization. Each tag consists of a key and an optional value. You
-- define both.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAddonResponse'
            Prelude.<$> (x Data..?> "addon")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAddon where
  hashWithSalt _salt CreateAddon' {..} =
    _salt `Prelude.hashWithSalt` addonVersion
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` configurationValues
      `Prelude.hashWithSalt` resolveConflicts
      `Prelude.hashWithSalt` serviceAccountRoleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` addonName

instance Prelude.NFData CreateAddon where
  rnf CreateAddon' {..} =
    Prelude.rnf addonVersion
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf configurationValues
      `Prelude.seq` Prelude.rnf resolveConflicts
      `Prelude.seq` Prelude.rnf serviceAccountRoleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf addonName

instance Data.ToHeaders CreateAddon where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAddon where
  toJSON CreateAddon' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addonVersion" Data..=) Prelude.<$> addonVersion,
            ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("configurationValues" Data..=)
              Prelude.<$> configurationValues,
            ("resolveConflicts" Data..=)
              Prelude.<$> resolveConflicts,
            ("serviceAccountRoleArn" Data..=)
              Prelude.<$> serviceAccountRoleArn,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("addonName" Data..= addonName)
          ]
      )

instance Data.ToPath CreateAddon where
  toPath CreateAddon' {..} =
    Prelude.mconcat
      ["/clusters/", Data.toBS clusterName, "/addons"]

instance Data.ToQuery CreateAddon where
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

instance Prelude.NFData CreateAddonResponse where
  rnf CreateAddonResponse' {..} =
    Prelude.rnf addon
      `Prelude.seq` Prelude.rnf httpStatus
