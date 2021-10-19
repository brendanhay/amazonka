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
-- Module      : Network.AWS.EKS.Types.Logging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Logging where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.LogSetup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the logging configuration for resources in your
-- cluster.
--
-- /See:/ 'newLogging' smart constructor.
data Logging = Logging'
  { -- | The cluster control plane logging configuration for your cluster.
    clusterLogging :: Prelude.Maybe [LogSetup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Logging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterLogging', 'logging_clusterLogging' - The cluster control plane logging configuration for your cluster.
newLogging ::
  Logging
newLogging =
  Logging' {clusterLogging = Prelude.Nothing}

-- | The cluster control plane logging configuration for your cluster.
logging_clusterLogging :: Lens.Lens' Logging (Prelude.Maybe [LogSetup])
logging_clusterLogging = Lens.lens (\Logging' {clusterLogging} -> clusterLogging) (\s@Logging' {} a -> s {clusterLogging = a} :: Logging) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Logging where
  parseJSON =
    Core.withObject
      "Logging"
      ( \x ->
          Logging'
            Prelude.<$> ( x Core..:? "clusterLogging"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Logging

instance Prelude.NFData Logging

instance Core.ToJSON Logging where
  toJSON Logging' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clusterLogging" Core..=)
              Prelude.<$> clusterLogging
          ]
      )
