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
-- Module      : Amazonka.ResilienceHub.Types.EksSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.EksSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input source of the Amazon Elastic Kubernetes Service cluster.
--
-- /See:/ 'newEksSource' smart constructor.
data EksSource = EksSource'
  { -- | The Amazon Resource Name (ARN) of the Amazon Elastic Kubernetes Service
    -- cluster. The format for this ARN is:
    -- arn:@aws@:eks:@region@:@account-id@:cluster\/@cluster-name@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    eksClusterArn :: Prelude.Text,
    -- | The list of namespaces located on your Amazon Elastic Kubernetes Service
    -- cluster.
    namespaces :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eksClusterArn', 'eksSource_eksClusterArn' - The Amazon Resource Name (ARN) of the Amazon Elastic Kubernetes Service
-- cluster. The format for this ARN is:
-- arn:@aws@:eks:@region@:@account-id@:cluster\/@cluster-name@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'namespaces', 'eksSource_namespaces' - The list of namespaces located on your Amazon Elastic Kubernetes Service
-- cluster.
newEksSource ::
  -- | 'eksClusterArn'
  Prelude.Text ->
  EksSource
newEksSource pEksClusterArn_ =
  EksSource'
    { eksClusterArn = pEksClusterArn_,
      namespaces = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the Amazon Elastic Kubernetes Service
-- cluster. The format for this ARN is:
-- arn:@aws@:eks:@region@:@account-id@:cluster\/@cluster-name@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
eksSource_eksClusterArn :: Lens.Lens' EksSource Prelude.Text
eksSource_eksClusterArn = Lens.lens (\EksSource' {eksClusterArn} -> eksClusterArn) (\s@EksSource' {} a -> s {eksClusterArn = a} :: EksSource)

-- | The list of namespaces located on your Amazon Elastic Kubernetes Service
-- cluster.
eksSource_namespaces :: Lens.Lens' EksSource [Prelude.Text]
eksSource_namespaces = Lens.lens (\EksSource' {namespaces} -> namespaces) (\s@EksSource' {} a -> s {namespaces = a} :: EksSource) Prelude.. Lens.coerced

instance Data.FromJSON EksSource where
  parseJSON =
    Data.withObject
      "EksSource"
      ( \x ->
          EksSource'
            Prelude.<$> (x Data..: "eksClusterArn")
            Prelude.<*> (x Data..:? "namespaces" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EksSource where
  hashWithSalt _salt EksSource' {..} =
    _salt
      `Prelude.hashWithSalt` eksClusterArn
      `Prelude.hashWithSalt` namespaces

instance Prelude.NFData EksSource where
  rnf EksSource' {..} =
    Prelude.rnf eksClusterArn
      `Prelude.seq` Prelude.rnf namespaces

instance Data.ToJSON EksSource where
  toJSON EksSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eksClusterArn" Data..= eksClusterArn),
            Prelude.Just ("namespaces" Data..= namespaces)
          ]
      )
