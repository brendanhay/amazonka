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
-- Module      : Amazonka.Batch.Types.EksConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration for the Amazon EKS cluster that supports the Batch compute
-- environment. The cluster must exist before the compute environment can
-- be created.
--
-- /See:/ 'newEksConfiguration' smart constructor.
data EksConfiguration = EksConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Amazon EKS cluster. An example is
    -- @arn:aws:eks:us-east-1:123456789012:cluster\/ClusterForBatch @.
    eksClusterArn :: Prelude.Text,
    -- | The namespace of the Amazon EKS cluster. Batch manages pods in this
    -- namespace. The value can\'t left empty or null. It must be fewer than 64
    -- characters long, can\'t be set to @default@, can\'t start with
    -- \"@kube-@,\" and must match this regular expression:
    -- @^[a-z0-9]([-a-z0-9]*[a-z0-9])?$@. For more information, see
    -- <https://kubernetes.io/docs/concepts/overview/working-with-objects/namespaces/ Namespaces>
    -- in the Kubernetes documentation.
    kubernetesNamespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eksClusterArn', 'eksConfiguration_eksClusterArn' - The Amazon Resource Name (ARN) of the Amazon EKS cluster. An example is
-- @arn:aws:eks:us-east-1:123456789012:cluster\/ClusterForBatch @.
--
-- 'kubernetesNamespace', 'eksConfiguration_kubernetesNamespace' - The namespace of the Amazon EKS cluster. Batch manages pods in this
-- namespace. The value can\'t left empty or null. It must be fewer than 64
-- characters long, can\'t be set to @default@, can\'t start with
-- \"@kube-@,\" and must match this regular expression:
-- @^[a-z0-9]([-a-z0-9]*[a-z0-9])?$@. For more information, see
-- <https://kubernetes.io/docs/concepts/overview/working-with-objects/namespaces/ Namespaces>
-- in the Kubernetes documentation.
newEksConfiguration ::
  -- | 'eksClusterArn'
  Prelude.Text ->
  -- | 'kubernetesNamespace'
  Prelude.Text ->
  EksConfiguration
newEksConfiguration
  pEksClusterArn_
  pKubernetesNamespace_ =
    EksConfiguration'
      { eksClusterArn = pEksClusterArn_,
        kubernetesNamespace = pKubernetesNamespace_
      }

-- | The Amazon Resource Name (ARN) of the Amazon EKS cluster. An example is
-- @arn:aws:eks:us-east-1:123456789012:cluster\/ClusterForBatch @.
eksConfiguration_eksClusterArn :: Lens.Lens' EksConfiguration Prelude.Text
eksConfiguration_eksClusterArn = Lens.lens (\EksConfiguration' {eksClusterArn} -> eksClusterArn) (\s@EksConfiguration' {} a -> s {eksClusterArn = a} :: EksConfiguration)

-- | The namespace of the Amazon EKS cluster. Batch manages pods in this
-- namespace. The value can\'t left empty or null. It must be fewer than 64
-- characters long, can\'t be set to @default@, can\'t start with
-- \"@kube-@,\" and must match this regular expression:
-- @^[a-z0-9]([-a-z0-9]*[a-z0-9])?$@. For more information, see
-- <https://kubernetes.io/docs/concepts/overview/working-with-objects/namespaces/ Namespaces>
-- in the Kubernetes documentation.
eksConfiguration_kubernetesNamespace :: Lens.Lens' EksConfiguration Prelude.Text
eksConfiguration_kubernetesNamespace = Lens.lens (\EksConfiguration' {kubernetesNamespace} -> kubernetesNamespace) (\s@EksConfiguration' {} a -> s {kubernetesNamespace = a} :: EksConfiguration)

instance Core.FromJSON EksConfiguration where
  parseJSON =
    Core.withObject
      "EksConfiguration"
      ( \x ->
          EksConfiguration'
            Prelude.<$> (x Core..: "eksClusterArn")
            Prelude.<*> (x Core..: "kubernetesNamespace")
      )

instance Prelude.Hashable EksConfiguration where
  hashWithSalt _salt EksConfiguration' {..} =
    _salt `Prelude.hashWithSalt` eksClusterArn
      `Prelude.hashWithSalt` kubernetesNamespace

instance Prelude.NFData EksConfiguration where
  rnf EksConfiguration' {..} =
    Prelude.rnf eksClusterArn
      `Prelude.seq` Prelude.rnf kubernetesNamespace

instance Core.ToJSON EksConfiguration where
  toJSON EksConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eksClusterArn" Core..= eksClusterArn),
            Prelude.Just
              ("kubernetesNamespace" Core..= kubernetesNamespace)
          ]
      )
