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
-- Module      : Network.AWS.IoT.Types.MitigationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationAction where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.MitigationActionParams
import qualified Network.AWS.Lens as Lens

-- | Describes which changes should be applied as part of a mitigation
-- action.
--
-- /See:/ 'newMitigationAction' smart constructor.
data MitigationAction = MitigationAction'
  { -- | The IAM role ARN used to apply this mitigation action.
    roleArn :: Core.Maybe Core.Text,
    -- | A unique identifier for the mitigation action.
    id :: Core.Maybe Core.Text,
    -- | The set of parameters for this mitigation action. The parameters vary,
    -- depending on the kind of action you apply.
    actionParams :: Core.Maybe MitigationActionParams,
    -- | A user-friendly name for the mitigation action.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MitigationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'mitigationAction_roleArn' - The IAM role ARN used to apply this mitigation action.
--
-- 'id', 'mitigationAction_id' - A unique identifier for the mitigation action.
--
-- 'actionParams', 'mitigationAction_actionParams' - The set of parameters for this mitigation action. The parameters vary,
-- depending on the kind of action you apply.
--
-- 'name', 'mitigationAction_name' - A user-friendly name for the mitigation action.
newMitigationAction ::
  MitigationAction
newMitigationAction =
  MitigationAction'
    { roleArn = Core.Nothing,
      id = Core.Nothing,
      actionParams = Core.Nothing,
      name = Core.Nothing
    }

-- | The IAM role ARN used to apply this mitigation action.
mitigationAction_roleArn :: Lens.Lens' MitigationAction (Core.Maybe Core.Text)
mitigationAction_roleArn = Lens.lens (\MitigationAction' {roleArn} -> roleArn) (\s@MitigationAction' {} a -> s {roleArn = a} :: MitigationAction)

-- | A unique identifier for the mitigation action.
mitigationAction_id :: Lens.Lens' MitigationAction (Core.Maybe Core.Text)
mitigationAction_id = Lens.lens (\MitigationAction' {id} -> id) (\s@MitigationAction' {} a -> s {id = a} :: MitigationAction)

-- | The set of parameters for this mitigation action. The parameters vary,
-- depending on the kind of action you apply.
mitigationAction_actionParams :: Lens.Lens' MitigationAction (Core.Maybe MitigationActionParams)
mitigationAction_actionParams = Lens.lens (\MitigationAction' {actionParams} -> actionParams) (\s@MitigationAction' {} a -> s {actionParams = a} :: MitigationAction)

-- | A user-friendly name for the mitigation action.
mitigationAction_name :: Lens.Lens' MitigationAction (Core.Maybe Core.Text)
mitigationAction_name = Lens.lens (\MitigationAction' {name} -> name) (\s@MitigationAction' {} a -> s {name = a} :: MitigationAction)

instance Core.FromJSON MitigationAction where
  parseJSON =
    Core.withObject
      "MitigationAction"
      ( \x ->
          MitigationAction'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "actionParams")
            Core.<*> (x Core..:? "name")
      )

instance Core.Hashable MitigationAction

instance Core.NFData MitigationAction
