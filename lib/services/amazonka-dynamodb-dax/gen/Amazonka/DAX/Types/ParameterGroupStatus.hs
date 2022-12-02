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
-- Module      : Amazonka.DAX.Types.ParameterGroupStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.ParameterGroupStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of a parameter group.
--
-- /See:/ 'newParameterGroupStatus' smart constructor.
data ParameterGroupStatus = ParameterGroupStatus'
  { -- | The name of the parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text,
    -- | The node IDs of one or more nodes to be rebooted.
    nodeIdsToReboot :: Prelude.Maybe [Prelude.Text]
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
-- 'parameterGroupName', 'parameterGroupStatus_parameterGroupName' - The name of the parameter group.
--
-- 'parameterApplyStatus', 'parameterGroupStatus_parameterApplyStatus' - The status of parameter updates.
--
-- 'nodeIdsToReboot', 'parameterGroupStatus_nodeIdsToReboot' - The node IDs of one or more nodes to be rebooted.
newParameterGroupStatus ::
  ParameterGroupStatus
newParameterGroupStatus =
  ParameterGroupStatus'
    { parameterGroupName =
        Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing,
      nodeIdsToReboot = Prelude.Nothing
    }

-- | The name of the parameter group.
parameterGroupStatus_parameterGroupName :: Lens.Lens' ParameterGroupStatus (Prelude.Maybe Prelude.Text)
parameterGroupStatus_parameterGroupName = Lens.lens (\ParameterGroupStatus' {parameterGroupName} -> parameterGroupName) (\s@ParameterGroupStatus' {} a -> s {parameterGroupName = a} :: ParameterGroupStatus)

-- | The status of parameter updates.
parameterGroupStatus_parameterApplyStatus :: Lens.Lens' ParameterGroupStatus (Prelude.Maybe Prelude.Text)
parameterGroupStatus_parameterApplyStatus = Lens.lens (\ParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@ParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: ParameterGroupStatus)

-- | The node IDs of one or more nodes to be rebooted.
parameterGroupStatus_nodeIdsToReboot :: Lens.Lens' ParameterGroupStatus (Prelude.Maybe [Prelude.Text])
parameterGroupStatus_nodeIdsToReboot = Lens.lens (\ParameterGroupStatus' {nodeIdsToReboot} -> nodeIdsToReboot) (\s@ParameterGroupStatus' {} a -> s {nodeIdsToReboot = a} :: ParameterGroupStatus) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ParameterGroupStatus where
  parseJSON =
    Data.withObject
      "ParameterGroupStatus"
      ( \x ->
          ParameterGroupStatus'
            Prelude.<$> (x Data..:? "ParameterGroupName")
            Prelude.<*> (x Data..:? "ParameterApplyStatus")
            Prelude.<*> ( x Data..:? "NodeIdsToReboot"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ParameterGroupStatus where
  hashWithSalt _salt ParameterGroupStatus' {..} =
    _salt `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` parameterApplyStatus
      `Prelude.hashWithSalt` nodeIdsToReboot

instance Prelude.NFData ParameterGroupStatus where
  rnf ParameterGroupStatus' {..} =
    Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf parameterApplyStatus
      `Prelude.seq` Prelude.rnf nodeIdsToReboot
