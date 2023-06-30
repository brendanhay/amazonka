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
-- Module      : Amazonka.EKS.Types.FargateProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.FargateProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.FargateProfileSelector
import Amazonka.EKS.Types.FargateProfileStatus
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Fargate profile.
--
-- /See:/ 'newFargateProfile' smart constructor.
data FargateProfile = FargateProfile'
  { -- | The name of the Amazon EKS cluster that the Fargate profile belongs to.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the Fargate profile was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The full Amazon Resource Name (ARN) of the Fargate profile.
    fargateProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Fargate profile.
    fargateProfileName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
    -- that match the selectors in the Fargate profile. For more information,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
    -- in the /Amazon EKS User Guide/.
    podExecutionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The selectors to match for pods to use this Fargate profile.
    selectors :: Prelude.Maybe [FargateProfileSelector],
    -- | The current status of the Fargate profile.
    status :: Prelude.Maybe FargateProfileStatus,
    -- | The IDs of subnets to launch pods into.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The metadata applied to the Fargate profile to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value. You define both. Fargate profile tags do not propagate
    -- to any other resources associated with the Fargate profile, such as the
    -- pods that are scheduled with it.
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
-- 'clusterName', 'fargateProfile_clusterName' - The name of the Amazon EKS cluster that the Fargate profile belongs to.
--
-- 'createdAt', 'fargateProfile_createdAt' - The Unix epoch timestamp in seconds for when the Fargate profile was
-- created.
--
-- 'fargateProfileArn', 'fargateProfile_fargateProfileArn' - The full Amazon Resource Name (ARN) of the Fargate profile.
--
-- 'fargateProfileName', 'fargateProfile_fargateProfileName' - The name of the Fargate profile.
--
-- 'podExecutionRoleArn', 'fargateProfile_podExecutionRoleArn' - The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
--
-- 'selectors', 'fargateProfile_selectors' - The selectors to match for pods to use this Fargate profile.
--
-- 'status', 'fargateProfile_status' - The current status of the Fargate profile.
--
-- 'subnets', 'fargateProfile_subnets' - The IDs of subnets to launch pods into.
--
-- 'tags', 'fargateProfile_tags' - The metadata applied to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value. You define both. Fargate profile tags do not propagate
-- to any other resources associated with the Fargate profile, such as the
-- pods that are scheduled with it.
newFargateProfile ::
  FargateProfile
newFargateProfile =
  FargateProfile'
    { clusterName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      fargateProfileArn = Prelude.Nothing,
      fargateProfileName = Prelude.Nothing,
      podExecutionRoleArn = Prelude.Nothing,
      selectors = Prelude.Nothing,
      status = Prelude.Nothing,
      subnets = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The name of the Amazon EKS cluster that the Fargate profile belongs to.
fargateProfile_clusterName :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_clusterName = Lens.lens (\FargateProfile' {clusterName} -> clusterName) (\s@FargateProfile' {} a -> s {clusterName = a} :: FargateProfile)

-- | The Unix epoch timestamp in seconds for when the Fargate profile was
-- created.
fargateProfile_createdAt :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.UTCTime)
fargateProfile_createdAt = Lens.lens (\FargateProfile' {createdAt} -> createdAt) (\s@FargateProfile' {} a -> s {createdAt = a} :: FargateProfile) Prelude.. Lens.mapping Data._Time

-- | The full Amazon Resource Name (ARN) of the Fargate profile.
fargateProfile_fargateProfileArn :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_fargateProfileArn = Lens.lens (\FargateProfile' {fargateProfileArn} -> fargateProfileArn) (\s@FargateProfile' {} a -> s {fargateProfileArn = a} :: FargateProfile)

-- | The name of the Fargate profile.
fargateProfile_fargateProfileName :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_fargateProfileName = Lens.lens (\FargateProfile' {fargateProfileName} -> fargateProfileName) (\s@FargateProfile' {} a -> s {fargateProfileName = a} :: FargateProfile)

-- | The Amazon Resource Name (ARN) of the pod execution role to use for pods
-- that match the selectors in the Fargate profile. For more information,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/pod-execution-role.html Pod Execution Role>
-- in the /Amazon EKS User Guide/.
fargateProfile_podExecutionRoleArn :: Lens.Lens' FargateProfile (Prelude.Maybe Prelude.Text)
fargateProfile_podExecutionRoleArn = Lens.lens (\FargateProfile' {podExecutionRoleArn} -> podExecutionRoleArn) (\s@FargateProfile' {} a -> s {podExecutionRoleArn = a} :: FargateProfile)

-- | The selectors to match for pods to use this Fargate profile.
fargateProfile_selectors :: Lens.Lens' FargateProfile (Prelude.Maybe [FargateProfileSelector])
fargateProfile_selectors = Lens.lens (\FargateProfile' {selectors} -> selectors) (\s@FargateProfile' {} a -> s {selectors = a} :: FargateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the Fargate profile.
fargateProfile_status :: Lens.Lens' FargateProfile (Prelude.Maybe FargateProfileStatus)
fargateProfile_status = Lens.lens (\FargateProfile' {status} -> status) (\s@FargateProfile' {} a -> s {status = a} :: FargateProfile)

-- | The IDs of subnets to launch pods into.
fargateProfile_subnets :: Lens.Lens' FargateProfile (Prelude.Maybe [Prelude.Text])
fargateProfile_subnets = Lens.lens (\FargateProfile' {subnets} -> subnets) (\s@FargateProfile' {} a -> s {subnets = a} :: FargateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The metadata applied to the Fargate profile to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value. You define both. Fargate profile tags do not propagate
-- to any other resources associated with the Fargate profile, such as the
-- pods that are scheduled with it.
fargateProfile_tags :: Lens.Lens' FargateProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
fargateProfile_tags = Lens.lens (\FargateProfile' {tags} -> tags) (\s@FargateProfile' {} a -> s {tags = a} :: FargateProfile) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FargateProfile where
  parseJSON =
    Data.withObject
      "FargateProfile"
      ( \x ->
          FargateProfile'
            Prelude.<$> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "fargateProfileArn")
            Prelude.<*> (x Data..:? "fargateProfileName")
            Prelude.<*> (x Data..:? "podExecutionRoleArn")
            Prelude.<*> (x Data..:? "selectors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "subnets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FargateProfile where
  hashWithSalt _salt FargateProfile' {..} =
    _salt
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` fargateProfileArn
      `Prelude.hashWithSalt` fargateProfileName
      `Prelude.hashWithSalt` podExecutionRoleArn
      `Prelude.hashWithSalt` selectors
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` tags

instance Prelude.NFData FargateProfile where
  rnf FargateProfile' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf fargateProfileArn
      `Prelude.seq` Prelude.rnf fargateProfileName
      `Prelude.seq` Prelude.rnf podExecutionRoleArn
      `Prelude.seq` Prelude.rnf selectors
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf tags
