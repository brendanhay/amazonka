{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EKS.Types.FargateProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.FargateProfile where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.FargateProfileSelector
import Network.AWS.EKS.Types.FargateProfileStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an Fargate profile.
--
-- /See:/ 'newFargateProfile' smart constructor.
data FargateProfile = FargateProfile'
  { -- | The full Amazon Resource Name (ARN) of the Fargate profile.
    fargateProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the Fargate profile.
    status :: Prelude.Maybe FargateProfileStatus,
    -- | The Unix epoch timestamp in seconds for when the Fargate profile was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The IDs of subnets to launch pods into.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Amazon EKS cluster that the Fargate profile belongs to.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
    -- that match the selectors in the Fargate profile. For more information,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
    -- in the /Amazon EKS User Guide/.
    podExecutionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Fargate profile.
    fargateProfileName :: Prelude.Maybe Prelude.Text,
    -- | The selectors to match for pods to use this Fargate profile.
    selectors :: Prelude.Maybe [FargateProfileSelector],
    -- | The metadata applied to the Fargate profile to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Fargate profile tags do not
    -- propagate to any other resources associated with the Fargate profile,
    -- such as the pods that are scheduled with it.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FargateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fargateProfileArn', 'fargateProfile_fargateProfileArn' - The full Amazon Resource Name (ARN) of the Fargate profile.
--
-- 'status', 'fargateProfile_status' - The current status of the Fargate profile.
--
-- 'createdAt', 'fargateProfile_createdAt' - The Unix epoch timestamp in seconds for when the Fargate profile was
-- created.
--
-- 'subnets', 'fargateProfile_subnets' - The IDs of subnets to launch pods into.
--
-- 'clusterName', 'fargateProfile_clusterName' - The name of the Amazon EKS cluster that the Fargate profile belongs to.
--
-- 'podExecutionRoleArn', 'fargateProfile_podExecutionRoleArn' - The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
--
-- 'fargateProfileName', 'fargateProfile_fargateProfileName' - The name of the Fargate profile.
--
-- 'selectors', 'fargateProfile_selectors' - The selectors to match for pods to use this Fargate profile.
--
-- 'tags', 'fargateProfile_tags' - The metadata applied to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Fargate profile tags do not
-- propagate to any other resources associated with the Fargate profile,
-- such as the pods that are scheduled with it.
newFargateProfile ::
  FargateProfile
newFargateProfile =
  FargateProfile'
    { fargateProfileArn =
        Prelude.Nothing,
      status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      subnets = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      podExecutionRoleArn = Prelude.Nothing,
      fargateProfileName = Prelude.Nothing,
      selectors = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The full Amazon Resource Name (ARN) of the Fargate profile.
fargateProfile_fargateProfileArn :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_fargateProfileArn = Lens.lens (\FargateProfile' {fargateProfileArn} -> fargateProfileArn) (\s@FargateProfile' {} a -> s {fargateProfileArn = a} :: FargateProfile)

-- | The current status of the Fargate profile.
fargateProfile_status :: Lens.Lens' FargateProfile (Prelude.Maybe FargateProfileStatus)
fargateProfile_status = Lens.lens (\FargateProfile' {status} -> status) (\s@FargateProfile' {} a -> s {status = a} :: FargateProfile)

-- | The Unix epoch timestamp in seconds for when the Fargate profile was
-- created.
fargateProfile_createdAt :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.UTCTime)
fargateProfile_createdAt = Lens.lens (\FargateProfile' {createdAt} -> createdAt) (\s@FargateProfile' {} a -> s {createdAt = a} :: FargateProfile) Prelude.. Lens.mapping Core._Time

-- | The IDs of subnets to launch pods into.
fargateProfile_subnets :: Lens.Lens' FargateProfile (Prelude.Maybe [Prelude.Text])
fargateProfile_subnets = Lens.lens (\FargateProfile' {subnets} -> subnets) (\s@FargateProfile' {} a -> s {subnets = a} :: FargateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon EKS cluster that the Fargate profile belongs to.
fargateProfile_clusterName :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_clusterName = Lens.lens (\FargateProfile' {clusterName} -> clusterName) (\s@FargateProfile' {} a -> s {clusterName = a} :: FargateProfile)

-- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
fargateProfile_podExecutionRoleArn :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_podExecutionRoleArn = Lens.lens (\FargateProfile' {podExecutionRoleArn} -> podExecutionRoleArn) (\s@FargateProfile' {} a -> s {podExecutionRoleArn = a} :: FargateProfile)

-- | The name of the Fargate profile.
fargateProfile_fargateProfileName :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_fargateProfileName = Lens.lens (\FargateProfile' {fargateProfileName} -> fargateProfileName) (\s@FargateProfile' {} a -> s {fargateProfileName = a} :: FargateProfile)

-- | The selectors to match for pods to use this Fargate profile.
fargateProfile_selectors :: Lens.Lens' FargateProfile (Prelude.Maybe [FargateProfileSelector])
fargateProfile_selectors = Lens.lens (\FargateProfile' {selectors} -> selectors) (\s@FargateProfile' {} a -> s {selectors = a} :: FargateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The metadata applied to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Fargate profile tags do not
-- propagate to any other resources associated with the Fargate profile,
-- such as the pods that are scheduled with it.
fargateProfile_tags :: Lens.Lens' FargateProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
fargateProfile_tags = Lens.lens (\FargateProfile' {tags} -> tags) (\s@FargateProfile' {} a -> s {tags = a} :: FargateProfile) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON FargateProfile where
  parseJSON =
    Core.withObject
      "FargateProfile"
      ( \x ->
          FargateProfile'
            Prelude.<$> (x Core..:? "fargateProfileArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "subnets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "clusterName")
            Prelude.<*> (x Core..:? "podExecutionRoleArn")
            Prelude.<*> (x Core..:? "fargateProfileName")
            Prelude.<*> (x Core..:? "selectors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable FargateProfile

instance Prelude.NFData FargateProfile
