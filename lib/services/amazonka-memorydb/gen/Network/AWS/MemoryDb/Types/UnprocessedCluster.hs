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
-- Module      : Network.AWS.MemoryDb.Types.UnprocessedCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Types.UnprocessedCluster where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A cluster whose updates have failed
--
-- /See:/ 'newUnprocessedCluster' smart constructor.
data UnprocessedCluster = UnprocessedCluster'
  { -- | The name of the cluster
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The error type associated with the update failure
    errorType :: Prelude.Maybe Prelude.Text,
    -- | The error message associated with the update failure
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'unprocessedCluster_clusterName' - The name of the cluster
--
-- 'errorType', 'unprocessedCluster_errorType' - The error type associated with the update failure
--
-- 'errorMessage', 'unprocessedCluster_errorMessage' - The error message associated with the update failure
newUnprocessedCluster ::
  UnprocessedCluster
newUnprocessedCluster =
  UnprocessedCluster'
    { clusterName = Prelude.Nothing,
      errorType = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The name of the cluster
unprocessedCluster_clusterName :: Lens.Lens' UnprocessedCluster (Prelude.Maybe Prelude.Text)
unprocessedCluster_clusterName = Lens.lens (\UnprocessedCluster' {clusterName} -> clusterName) (\s@UnprocessedCluster' {} a -> s {clusterName = a} :: UnprocessedCluster)

-- | The error type associated with the update failure
unprocessedCluster_errorType :: Lens.Lens' UnprocessedCluster (Prelude.Maybe Prelude.Text)
unprocessedCluster_errorType = Lens.lens (\UnprocessedCluster' {errorType} -> errorType) (\s@UnprocessedCluster' {} a -> s {errorType = a} :: UnprocessedCluster)

-- | The error message associated with the update failure
unprocessedCluster_errorMessage :: Lens.Lens' UnprocessedCluster (Prelude.Maybe Prelude.Text)
unprocessedCluster_errorMessage = Lens.lens (\UnprocessedCluster' {errorMessage} -> errorMessage) (\s@UnprocessedCluster' {} a -> s {errorMessage = a} :: UnprocessedCluster)

instance Core.FromJSON UnprocessedCluster where
  parseJSON =
    Core.withObject
      "UnprocessedCluster"
      ( \x ->
          UnprocessedCluster'
            Prelude.<$> (x Core..:? "ClusterName")
            Prelude.<*> (x Core..:? "ErrorType")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable UnprocessedCluster

instance Prelude.NFData UnprocessedCluster
