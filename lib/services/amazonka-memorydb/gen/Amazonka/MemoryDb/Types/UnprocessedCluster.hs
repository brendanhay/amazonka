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
-- Module      : Amazonka.MemoryDb.Types.UnprocessedCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.UnprocessedCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A cluster whose updates have failed
--
-- /See:/ 'newUnprocessedCluster' smart constructor.
data UnprocessedCluster = UnprocessedCluster'
  { -- | The name of the cluster
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The error message associated with the update failure
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error type associated with the update failure
    errorType :: Prelude.Maybe Prelude.Text
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
-- 'errorMessage', 'unprocessedCluster_errorMessage' - The error message associated with the update failure
--
-- 'errorType', 'unprocessedCluster_errorType' - The error type associated with the update failure
newUnprocessedCluster ::
  UnprocessedCluster
newUnprocessedCluster =
  UnprocessedCluster'
    { clusterName = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorType = Prelude.Nothing
    }

-- | The name of the cluster
unprocessedCluster_clusterName :: Lens.Lens' UnprocessedCluster (Prelude.Maybe Prelude.Text)
unprocessedCluster_clusterName = Lens.lens (\UnprocessedCluster' {clusterName} -> clusterName) (\s@UnprocessedCluster' {} a -> s {clusterName = a} :: UnprocessedCluster)

-- | The error message associated with the update failure
unprocessedCluster_errorMessage :: Lens.Lens' UnprocessedCluster (Prelude.Maybe Prelude.Text)
unprocessedCluster_errorMessage = Lens.lens (\UnprocessedCluster' {errorMessage} -> errorMessage) (\s@UnprocessedCluster' {} a -> s {errorMessage = a} :: UnprocessedCluster)

-- | The error type associated with the update failure
unprocessedCluster_errorType :: Lens.Lens' UnprocessedCluster (Prelude.Maybe Prelude.Text)
unprocessedCluster_errorType = Lens.lens (\UnprocessedCluster' {errorType} -> errorType) (\s@UnprocessedCluster' {} a -> s {errorType = a} :: UnprocessedCluster)

instance Data.FromJSON UnprocessedCluster where
  parseJSON =
    Data.withObject
      "UnprocessedCluster"
      ( \x ->
          UnprocessedCluster'
            Prelude.<$> (x Data..:? "ClusterName")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ErrorType")
      )

instance Prelude.Hashable UnprocessedCluster where
  hashWithSalt _salt UnprocessedCluster' {..} =
    _salt
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorType

instance Prelude.NFData UnprocessedCluster where
  rnf UnprocessedCluster' {..} =
    Prelude.rnf clusterName `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf errorType
