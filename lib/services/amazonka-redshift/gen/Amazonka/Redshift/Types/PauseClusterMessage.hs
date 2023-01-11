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
-- Module      : Amazonka.Redshift.Types.PauseClusterMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.PauseClusterMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a pause cluster operation. For example, a scheduled action to
-- run the @PauseCluster@ API operation.
--
-- /See:/ 'newPauseClusterMessage' smart constructor.
data PauseClusterMessage = PauseClusterMessage'
  { -- | The identifier of the cluster to be paused.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PauseClusterMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'pauseClusterMessage_clusterIdentifier' - The identifier of the cluster to be paused.
newPauseClusterMessage ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  PauseClusterMessage
newPauseClusterMessage pClusterIdentifier_ =
  PauseClusterMessage'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster to be paused.
pauseClusterMessage_clusterIdentifier :: Lens.Lens' PauseClusterMessage Prelude.Text
pauseClusterMessage_clusterIdentifier = Lens.lens (\PauseClusterMessage' {clusterIdentifier} -> clusterIdentifier) (\s@PauseClusterMessage' {} a -> s {clusterIdentifier = a} :: PauseClusterMessage)

instance Data.FromXML PauseClusterMessage where
  parseXML x =
    PauseClusterMessage'
      Prelude.<$> (x Data..@ "ClusterIdentifier")

instance Prelude.Hashable PauseClusterMessage where
  hashWithSalt _salt PauseClusterMessage' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData PauseClusterMessage where
  rnf PauseClusterMessage' {..} =
    Prelude.rnf clusterIdentifier

instance Data.ToQuery PauseClusterMessage where
  toQuery PauseClusterMessage' {..} =
    Prelude.mconcat
      ["ClusterIdentifier" Data.=: clusterIdentifier]
