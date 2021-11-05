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
-- Module      : Amazonka.Redshift.Types.ClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterParameterGroupNameMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- |
--
-- /See:/ 'newClusterParameterGroupNameMessage' smart constructor.
data ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage'
  { -- | The status of the parameter group. For example, if you made a change to
    -- a parameter group name-value pair, then the change could be pending a
    -- reboot of an associated cluster.
    parameterGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterParameterGroupNameMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupStatus', 'clusterParameterGroupNameMessage_parameterGroupStatus' - The status of the parameter group. For example, if you made a change to
-- a parameter group name-value pair, then the change could be pending a
-- reboot of an associated cluster.
--
-- 'parameterGroupName', 'clusterParameterGroupNameMessage_parameterGroupName' - The name of the cluster parameter group.
newClusterParameterGroupNameMessage ::
  ClusterParameterGroupNameMessage
newClusterParameterGroupNameMessage =
  ClusterParameterGroupNameMessage'
    { parameterGroupStatus =
        Prelude.Nothing,
      parameterGroupName = Prelude.Nothing
    }

-- | The status of the parameter group. For example, if you made a change to
-- a parameter group name-value pair, then the change could be pending a
-- reboot of an associated cluster.
clusterParameterGroupNameMessage_parameterGroupStatus :: Lens.Lens' ClusterParameterGroupNameMessage (Prelude.Maybe Prelude.Text)
clusterParameterGroupNameMessage_parameterGroupStatus = Lens.lens (\ClusterParameterGroupNameMessage' {parameterGroupStatus} -> parameterGroupStatus) (\s@ClusterParameterGroupNameMessage' {} a -> s {parameterGroupStatus = a} :: ClusterParameterGroupNameMessage)

-- | The name of the cluster parameter group.
clusterParameterGroupNameMessage_parameterGroupName :: Lens.Lens' ClusterParameterGroupNameMessage (Prelude.Maybe Prelude.Text)
clusterParameterGroupNameMessage_parameterGroupName = Lens.lens (\ClusterParameterGroupNameMessage' {parameterGroupName} -> parameterGroupName) (\s@ClusterParameterGroupNameMessage' {} a -> s {parameterGroupName = a} :: ClusterParameterGroupNameMessage)

instance
  Core.FromXML
    ClusterParameterGroupNameMessage
  where
  parseXML x =
    ClusterParameterGroupNameMessage'
      Prelude.<$> (x Core..@? "ParameterGroupStatus")
      Prelude.<*> (x Core..@? "ParameterGroupName")

instance
  Prelude.Hashable
    ClusterParameterGroupNameMessage

instance
  Prelude.NFData
    ClusterParameterGroupNameMessage
