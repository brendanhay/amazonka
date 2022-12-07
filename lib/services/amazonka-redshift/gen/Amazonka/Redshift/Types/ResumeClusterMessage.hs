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
-- Module      : Amazonka.Redshift.Types.ResumeClusterMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ResumeClusterMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a resume cluster operation. For example, a scheduled action to
-- run the @ResumeCluster@ API operation.
--
-- /See:/ 'newResumeClusterMessage' smart constructor.
data ResumeClusterMessage = ResumeClusterMessage'
  { -- | The identifier of the cluster to be resumed.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeClusterMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'resumeClusterMessage_clusterIdentifier' - The identifier of the cluster to be resumed.
newResumeClusterMessage ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ResumeClusterMessage
newResumeClusterMessage pClusterIdentifier_ =
  ResumeClusterMessage'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster to be resumed.
resumeClusterMessage_clusterIdentifier :: Lens.Lens' ResumeClusterMessage Prelude.Text
resumeClusterMessage_clusterIdentifier = Lens.lens (\ResumeClusterMessage' {clusterIdentifier} -> clusterIdentifier) (\s@ResumeClusterMessage' {} a -> s {clusterIdentifier = a} :: ResumeClusterMessage)

instance Data.FromXML ResumeClusterMessage where
  parseXML x =
    ResumeClusterMessage'
      Prelude.<$> (x Data..@ "ClusterIdentifier")

instance Prelude.Hashable ResumeClusterMessage where
  hashWithSalt _salt ResumeClusterMessage' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData ResumeClusterMessage where
  rnf ResumeClusterMessage' {..} =
    Prelude.rnf clusterIdentifier

instance Data.ToQuery ResumeClusterMessage where
  toQuery ResumeClusterMessage' {..} =
    Prelude.mconcat
      ["ClusterIdentifier" Data.=: clusterIdentifier]
