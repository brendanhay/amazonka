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
-- Module      : Amazonka.ResilienceHub.Types.EksSourceClusterNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.EksSourceClusterNamespace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input source of the namespace that is located on your Amazon Elastic
-- Kubernetes Service cluster.
--
-- /See:/ 'newEksSourceClusterNamespace' smart constructor.
data EksSourceClusterNamespace = EksSourceClusterNamespace'
  { -- | The Amazon Resource Name (ARN) of the Amazon Elastic Kubernetes Service
    -- cluster. The format for this ARN is:
    -- arn:@aws@:eks:@region@:@account-id@:cluster\/@cluster-name@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    eksClusterArn :: Prelude.Text,
    -- | Name of the namespace that is located on your Amazon Elastic Kubernetes
    -- Service cluster.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksSourceClusterNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eksClusterArn', 'eksSourceClusterNamespace_eksClusterArn' - The Amazon Resource Name (ARN) of the Amazon Elastic Kubernetes Service
-- cluster. The format for this ARN is:
-- arn:@aws@:eks:@region@:@account-id@:cluster\/@cluster-name@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'namespace', 'eksSourceClusterNamespace_namespace' - Name of the namespace that is located on your Amazon Elastic Kubernetes
-- Service cluster.
newEksSourceClusterNamespace ::
  -- | 'eksClusterArn'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  EksSourceClusterNamespace
newEksSourceClusterNamespace
  pEksClusterArn_
  pNamespace_ =
    EksSourceClusterNamespace'
      { eksClusterArn =
          pEksClusterArn_,
        namespace = pNamespace_
      }

-- | The Amazon Resource Name (ARN) of the Amazon Elastic Kubernetes Service
-- cluster. The format for this ARN is:
-- arn:@aws@:eks:@region@:@account-id@:cluster\/@cluster-name@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
eksSourceClusterNamespace_eksClusterArn :: Lens.Lens' EksSourceClusterNamespace Prelude.Text
eksSourceClusterNamespace_eksClusterArn = Lens.lens (\EksSourceClusterNamespace' {eksClusterArn} -> eksClusterArn) (\s@EksSourceClusterNamespace' {} a -> s {eksClusterArn = a} :: EksSourceClusterNamespace)

-- | Name of the namespace that is located on your Amazon Elastic Kubernetes
-- Service cluster.
eksSourceClusterNamespace_namespace :: Lens.Lens' EksSourceClusterNamespace Prelude.Text
eksSourceClusterNamespace_namespace = Lens.lens (\EksSourceClusterNamespace' {namespace} -> namespace) (\s@EksSourceClusterNamespace' {} a -> s {namespace = a} :: EksSourceClusterNamespace)

instance Data.FromJSON EksSourceClusterNamespace where
  parseJSON =
    Data.withObject
      "EksSourceClusterNamespace"
      ( \x ->
          EksSourceClusterNamespace'
            Prelude.<$> (x Data..: "eksClusterArn")
            Prelude.<*> (x Data..: "namespace")
      )

instance Prelude.Hashable EksSourceClusterNamespace where
  hashWithSalt _salt EksSourceClusterNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` eksClusterArn
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData EksSourceClusterNamespace where
  rnf EksSourceClusterNamespace' {..} =
    Prelude.rnf eksClusterArn
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToJSON EksSourceClusterNamespace where
  toJSON EksSourceClusterNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eksClusterArn" Data..= eksClusterArn),
            Prelude.Just ("namespace" Data..= namespace)
          ]
      )
