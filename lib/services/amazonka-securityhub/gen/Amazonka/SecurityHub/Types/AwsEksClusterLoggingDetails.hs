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
-- Module      : Amazonka.SecurityHub.Types.AwsEksClusterLoggingDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEksClusterLoggingDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEksClusterLoggingClusterLoggingDetails

-- | The logging configuration for an Amazon EKS cluster.
--
-- /See:/ 'newAwsEksClusterLoggingDetails' smart constructor.
data AwsEksClusterLoggingDetails = AwsEksClusterLoggingDetails'
  { -- | Cluster logging configurations.
    clusterLogging :: Prelude.Maybe [AwsEksClusterLoggingClusterLoggingDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEksClusterLoggingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterLogging', 'awsEksClusterLoggingDetails_clusterLogging' - Cluster logging configurations.
newAwsEksClusterLoggingDetails ::
  AwsEksClusterLoggingDetails
newAwsEksClusterLoggingDetails =
  AwsEksClusterLoggingDetails'
    { clusterLogging =
        Prelude.Nothing
    }

-- | Cluster logging configurations.
awsEksClusterLoggingDetails_clusterLogging :: Lens.Lens' AwsEksClusterLoggingDetails (Prelude.Maybe [AwsEksClusterLoggingClusterLoggingDetails])
awsEksClusterLoggingDetails_clusterLogging = Lens.lens (\AwsEksClusterLoggingDetails' {clusterLogging} -> clusterLogging) (\s@AwsEksClusterLoggingDetails' {} a -> s {clusterLogging = a} :: AwsEksClusterLoggingDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsEksClusterLoggingDetails where
  parseJSON =
    Core.withObject
      "AwsEksClusterLoggingDetails"
      ( \x ->
          AwsEksClusterLoggingDetails'
            Prelude.<$> ( x Core..:? "ClusterLogging"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsEksClusterLoggingDetails where
  hashWithSalt _salt AwsEksClusterLoggingDetails' {..} =
    _salt `Prelude.hashWithSalt` clusterLogging

instance Prelude.NFData AwsEksClusterLoggingDetails where
  rnf AwsEksClusterLoggingDetails' {..} =
    Prelude.rnf clusterLogging

instance Core.ToJSON AwsEksClusterLoggingDetails where
  toJSON AwsEksClusterLoggingDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClusterLogging" Core..=)
              Prelude.<$> clusterLogging
          ]
      )
