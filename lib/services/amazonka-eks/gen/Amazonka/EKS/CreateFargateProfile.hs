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
-- Module      : Amazonka.EKS.CreateFargateProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Fargate profile for your Amazon EKS cluster. You must have at
-- least one Fargate profile in a cluster to be able to run pods on
-- Fargate.
--
-- The Fargate profile allows an administrator to declare which pods run on
-- Fargate and specify which pods run on which Fargate profile. This
-- declaration is done through the profile’s selectors. Each profile can
-- have up to five selectors that contain a namespace and labels. A
-- namespace is required for every selector. The label field consists of
-- multiple optional key-value pairs. Pods that match the selectors are
-- scheduled on Fargate. If a to-be-scheduled pod matches any of the
-- selectors in the Fargate profile, then that pod is run on Fargate.
--
-- When you create a Fargate profile, you must specify a pod execution role
-- to use with the pods that are scheduled with the profile. This role is
-- added to the cluster\'s Kubernetes
-- <https://kubernetes.io/docs/admin/authorization/rbac/ Role Based Access Control>
-- (RBAC) for authorization so that the @kubelet@ that is running on the
-- Fargate infrastructure can register with your Amazon EKS cluster so that
-- it can appear in your cluster as a node. The pod execution role also
-- provides IAM permissions to the Fargate infrastructure to allow read
-- access to Amazon ECR image repositories. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
--
-- Fargate profiles are immutable. However, you can create a new updated
-- profile to replace an existing profile and then delete the original
-- after the updated profile has finished creating.
--
-- If any Fargate profiles in a cluster are in the @DELETING@ status, you
-- must wait for that Fargate profile to finish deleting before you can
-- create any other profiles in that cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/fargate-profile.html Fargate Profile>
-- in the /Amazon EKS User Guide/.
module Amazonka.EKS.CreateFargateProfile
  ( -- * Creating a Request
    CreateFargateProfile (..),
    newCreateFargateProfile,

    -- * Request Lenses
    createFargateProfile_tags,
    createFargateProfile_clientRequestToken,
    createFargateProfile_subnets,
    createFargateProfile_selectors,
    createFargateProfile_fargateProfileName,
    createFargateProfile_clusterName,
    createFargateProfile_podExecutionRoleArn,

    -- * Destructuring the Response
    CreateFargateProfileResponse (..),
    newCreateFargateProfileResponse,

    -- * Response Lenses
    createFargateProfileResponse_fargateProfile,
    createFargateProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFargateProfile' smart constructor.
data CreateFargateProfile = CreateFargateProfile'
  { -- | The metadata to apply to the Fargate profile to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value. You define both. Fargate profile tags do not propagate
    -- to any other resources associated with the Fargate profile, such as the
    -- pods that are scheduled with it.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of subnets to launch your pods into. At this time, pods running
    -- on Fargate are not assigned public IP addresses, so only private subnets
    -- (with no direct route to an Internet Gateway) are accepted for this
    -- parameter.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The selectors to match for pods to use this Fargate profile. Each
    -- selector must have an associated namespace. Optionally, you can also
    -- specify labels for a namespace. You may specify up to five selectors in
    -- a Fargate profile.
    selectors :: Prelude.Maybe [FargateProfileSelector],
    -- | The name of the Fargate profile.
    fargateProfileName :: Prelude.Text,
    -- | The name of the Amazon EKS cluster to apply the Fargate profile to.
    clusterName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
    -- that match the selectors in the Fargate profile. The pod execution role
    -- allows Fargate infrastructure to register with your cluster as a node,
    -- and it provides read access to Amazon ECR image repositories. For more
    -- information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
    -- in the /Amazon EKS User Guide/.
    podExecutionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFargateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFargateProfile_tags' - The metadata to apply to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value. You define both. Fargate profile tags do not propagate
-- to any other resources associated with the Fargate profile, such as the
-- pods that are scheduled with it.
--
-- 'clientRequestToken', 'createFargateProfile_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'subnets', 'createFargateProfile_subnets' - The IDs of subnets to launch your pods into. At this time, pods running
-- on Fargate are not assigned public IP addresses, so only private subnets
-- (with no direct route to an Internet Gateway) are accepted for this
-- parameter.
--
-- 'selectors', 'createFargateProfile_selectors' - The selectors to match for pods to use this Fargate profile. Each
-- selector must have an associated namespace. Optionally, you can also
-- specify labels for a namespace. You may specify up to five selectors in
-- a Fargate profile.
--
-- 'fargateProfileName', 'createFargateProfile_fargateProfileName' - The name of the Fargate profile.
--
-- 'clusterName', 'createFargateProfile_clusterName' - The name of the Amazon EKS cluster to apply the Fargate profile to.
--
-- 'podExecutionRoleArn', 'createFargateProfile_podExecutionRoleArn' - The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. The pod execution role
-- allows Fargate infrastructure to register with your cluster as a node,
-- and it provides read access to Amazon ECR image repositories. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
newCreateFargateProfile ::
  -- | 'fargateProfileName'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'podExecutionRoleArn'
  Prelude.Text ->
  CreateFargateProfile
newCreateFargateProfile
  pFargateProfileName_
  pClusterName_
  pPodExecutionRoleArn_ =
    CreateFargateProfile'
      { tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        subnets = Prelude.Nothing,
        selectors = Prelude.Nothing,
        fargateProfileName = pFargateProfileName_,
        clusterName = pClusterName_,
        podExecutionRoleArn = pPodExecutionRoleArn_
      }

-- | The metadata to apply to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value. You define both. Fargate profile tags do not propagate
-- to any other resources associated with the Fargate profile, such as the
-- pods that are scheduled with it.
createFargateProfile_tags :: Lens.Lens' CreateFargateProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFargateProfile_tags = Lens.lens (\CreateFargateProfile' {tags} -> tags) (\s@CreateFargateProfile' {} a -> s {tags = a} :: CreateFargateProfile) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createFargateProfile_clientRequestToken :: Lens.Lens' CreateFargateProfile (Prelude.Maybe Prelude.Text)
createFargateProfile_clientRequestToken = Lens.lens (\CreateFargateProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateFargateProfile' {} a -> s {clientRequestToken = a} :: CreateFargateProfile)

-- | The IDs of subnets to launch your pods into. At this time, pods running
-- on Fargate are not assigned public IP addresses, so only private subnets
-- (with no direct route to an Internet Gateway) are accepted for this
-- parameter.
createFargateProfile_subnets :: Lens.Lens' CreateFargateProfile (Prelude.Maybe [Prelude.Text])
createFargateProfile_subnets = Lens.lens (\CreateFargateProfile' {subnets} -> subnets) (\s@CreateFargateProfile' {} a -> s {subnets = a} :: CreateFargateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The selectors to match for pods to use this Fargate profile. Each
-- selector must have an associated namespace. Optionally, you can also
-- specify labels for a namespace. You may specify up to five selectors in
-- a Fargate profile.
createFargateProfile_selectors :: Lens.Lens' CreateFargateProfile (Prelude.Maybe [FargateProfileSelector])
createFargateProfile_selectors = Lens.lens (\CreateFargateProfile' {selectors} -> selectors) (\s@CreateFargateProfile' {} a -> s {selectors = a} :: CreateFargateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Fargate profile.
createFargateProfile_fargateProfileName :: Lens.Lens' CreateFargateProfile Prelude.Text
createFargateProfile_fargateProfileName = Lens.lens (\CreateFargateProfile' {fargateProfileName} -> fargateProfileName) (\s@CreateFargateProfile' {} a -> s {fargateProfileName = a} :: CreateFargateProfile)

-- | The name of the Amazon EKS cluster to apply the Fargate profile to.
createFargateProfile_clusterName :: Lens.Lens' CreateFargateProfile Prelude.Text
createFargateProfile_clusterName = Lens.lens (\CreateFargateProfile' {clusterName} -> clusterName) (\s@CreateFargateProfile' {} a -> s {clusterName = a} :: CreateFargateProfile)

-- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. The pod execution role
-- allows Fargate infrastructure to register with your cluster as a node,
-- and it provides read access to Amazon ECR image repositories. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
createFargateProfile_podExecutionRoleArn :: Lens.Lens' CreateFargateProfile Prelude.Text
createFargateProfile_podExecutionRoleArn = Lens.lens (\CreateFargateProfile' {podExecutionRoleArn} -> podExecutionRoleArn) (\s@CreateFargateProfile' {} a -> s {podExecutionRoleArn = a} :: CreateFargateProfile)

instance Core.AWSRequest CreateFargateProfile where
  type
    AWSResponse CreateFargateProfile =
      CreateFargateProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFargateProfileResponse'
            Prelude.<$> (x Core..?> "fargateProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFargateProfile where
  hashWithSalt _salt CreateFargateProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` selectors
      `Prelude.hashWithSalt` fargateProfileName
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` podExecutionRoleArn

instance Prelude.NFData CreateFargateProfile where
  rnf CreateFargateProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf selectors
      `Prelude.seq` Prelude.rnf fargateProfileName
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf podExecutionRoleArn

instance Core.ToHeaders CreateFargateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFargateProfile where
  toJSON CreateFargateProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("subnets" Core..=) Prelude.<$> subnets,
            ("selectors" Core..=) Prelude.<$> selectors,
            Prelude.Just
              ("fargateProfileName" Core..= fargateProfileName),
            Prelude.Just
              ("podExecutionRoleArn" Core..= podExecutionRoleArn)
          ]
      )

instance Core.ToPath CreateFargateProfile where
  toPath CreateFargateProfile' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/fargate-profiles"
      ]

instance Core.ToQuery CreateFargateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFargateProfileResponse' smart constructor.
data CreateFargateProfileResponse = CreateFargateProfileResponse'
  { -- | The full description of your new Fargate profile.
    fargateProfile :: Prelude.Maybe FargateProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFargateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fargateProfile', 'createFargateProfileResponse_fargateProfile' - The full description of your new Fargate profile.
--
-- 'httpStatus', 'createFargateProfileResponse_httpStatus' - The response's http status code.
newCreateFargateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFargateProfileResponse
newCreateFargateProfileResponse pHttpStatus_ =
  CreateFargateProfileResponse'
    { fargateProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your new Fargate profile.
createFargateProfileResponse_fargateProfile :: Lens.Lens' CreateFargateProfileResponse (Prelude.Maybe FargateProfile)
createFargateProfileResponse_fargateProfile = Lens.lens (\CreateFargateProfileResponse' {fargateProfile} -> fargateProfile) (\s@CreateFargateProfileResponse' {} a -> s {fargateProfile = a} :: CreateFargateProfileResponse)

-- | The response's http status code.
createFargateProfileResponse_httpStatus :: Lens.Lens' CreateFargateProfileResponse Prelude.Int
createFargateProfileResponse_httpStatus = Lens.lens (\CreateFargateProfileResponse' {httpStatus} -> httpStatus) (\s@CreateFargateProfileResponse' {} a -> s {httpStatus = a} :: CreateFargateProfileResponse)

instance Prelude.NFData CreateFargateProfileResponse where
  rnf CreateFargateProfileResponse' {..} =
    Prelude.rnf fargateProfile
      `Prelude.seq` Prelude.rnf httpStatus
