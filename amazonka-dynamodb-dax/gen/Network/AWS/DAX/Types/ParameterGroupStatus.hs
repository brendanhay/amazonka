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
-- Module      : Network.AWS.DAX.Types.ParameterGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterGroupStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of a parameter group.
--
-- /See:/ 'newParameterGroupStatus' smart constructor.
data ParameterGroupStatus = ParameterGroupStatus'
  { -- | The node IDs of one or more nodes to be rebooted.
    nodeIdsToReboot :: Prelude.Maybe [Prelude.Text],
    -- | The name of the parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeIdsToReboot', 'parameterGroupStatus_nodeIdsToReboot' - The node IDs of one or more nodes to be rebooted.
--
-- 'parameterGroupName', 'parameterGroupStatus_parameterGroupName' - The name of the parameter group.
--
-- 'parameterApplyStatus', 'parameterGroupStatus_parameterApplyStatus' - The status of parameter updates.
newParameterGroupStatus ::
  ParameterGroupStatus
newParameterGroupStatus =
  ParameterGroupStatus'
    { nodeIdsToReboot =
        Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing
    }

-- | The node IDs of one or more nodes to be rebooted.
parameterGroupStatus_nodeIdsToReboot :: Lens.Lens' ParameterGroupStatus (Prelude.Maybe [Prelude.Text])
parameterGroupStatus_nodeIdsToReboot = Lens.lens (\ParameterGroupStatus' {nodeIdsToReboot} -> nodeIdsToReboot) (\s@ParameterGroupStatus' {} a -> s {nodeIdsToReboot = a} :: ParameterGroupStatus) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the parameter group.
parameterGroupStatus_parameterGroupName :: Lens.Lens' ParameterGroupStatus (Prelude.Maybe Prelude.Text)
parameterGroupStatus_parameterGroupName = Lens.lens (\ParameterGroupStatus' {parameterGroupName} -> parameterGroupName) (\s@ParameterGroupStatus' {} a -> s {parameterGroupName = a} :: ParameterGroupStatus)

-- | The status of parameter updates.
parameterGroupStatus_parameterApplyStatus :: Lens.Lens' ParameterGroupStatus (Prelude.Maybe Prelude.Text)
parameterGroupStatus_parameterApplyStatus = Lens.lens (\ParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@ParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: ParameterGroupStatus)

instance Core.FromJSON ParameterGroupStatus where
  parseJSON =
    Core.withObject
      "ParameterGroupStatus"
      ( \x ->
          ParameterGroupStatus'
            Prelude.<$> ( x Core..:? "NodeIdsToReboot"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ParameterGroupName")
            Prelude.<*> (x Core..:? "ParameterApplyStatus")
      )

instance Prelude.Hashable ParameterGroupStatus

instance Prelude.NFData ParameterGroupStatus
