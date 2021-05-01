{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types.MitigationActionParams
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes which changes should be applied as part of a mitigation
-- action.
--
-- /See:/ 'newMitigationAction' smart constructor.
data MitigationAction = MitigationAction'
  { -- | The IAM role ARN used to apply this mitigation action.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the mitigation action.
    id :: Prelude.Maybe Prelude.Text,
    -- | The set of parameters for this mitigation action. The parameters vary,
    -- depending on the kind of action you apply.
    actionParams :: Prelude.Maybe MitigationActionParams,
    -- | A user-friendly name for the mitigation action.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { roleArn = Prelude.Nothing,
      id = Prelude.Nothing,
      actionParams = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The IAM role ARN used to apply this mitigation action.
mitigationAction_roleArn :: Lens.Lens' MitigationAction (Prelude.Maybe Prelude.Text)
mitigationAction_roleArn = Lens.lens (\MitigationAction' {roleArn} -> roleArn) (\s@MitigationAction' {} a -> s {roleArn = a} :: MitigationAction)

-- | A unique identifier for the mitigation action.
mitigationAction_id :: Lens.Lens' MitigationAction (Prelude.Maybe Prelude.Text)
mitigationAction_id = Lens.lens (\MitigationAction' {id} -> id) (\s@MitigationAction' {} a -> s {id = a} :: MitigationAction)

-- | The set of parameters for this mitigation action. The parameters vary,
-- depending on the kind of action you apply.
mitigationAction_actionParams :: Lens.Lens' MitigationAction (Prelude.Maybe MitigationActionParams)
mitigationAction_actionParams = Lens.lens (\MitigationAction' {actionParams} -> actionParams) (\s@MitigationAction' {} a -> s {actionParams = a} :: MitigationAction)

-- | A user-friendly name for the mitigation action.
mitigationAction_name :: Lens.Lens' MitigationAction (Prelude.Maybe Prelude.Text)
mitigationAction_name = Lens.lens (\MitigationAction' {name} -> name) (\s@MitigationAction' {} a -> s {name = a} :: MitigationAction)

instance Prelude.FromJSON MitigationAction where
  parseJSON =
    Prelude.withObject
      "MitigationAction"
      ( \x ->
          MitigationAction'
            Prelude.<$> (x Prelude..:? "roleArn")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "actionParams")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable MitigationAction

instance Prelude.NFData MitigationAction
