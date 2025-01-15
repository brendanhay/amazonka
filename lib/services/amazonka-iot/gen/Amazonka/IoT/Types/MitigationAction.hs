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
-- Module      : Amazonka.IoT.Types.MitigationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MitigationAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.MitigationActionParams
import qualified Amazonka.Prelude as Prelude

-- | Describes which changes should be applied as part of a mitigation
-- action.
--
-- /See:/ 'newMitigationAction' smart constructor.
data MitigationAction = MitigationAction'
  { -- | The set of parameters for this mitigation action. The parameters vary,
    -- depending on the kind of action you apply.
    actionParams :: Prelude.Maybe MitigationActionParams,
    -- | A unique identifier for the mitigation action.
    id :: Prelude.Maybe Prelude.Text,
    -- | A user-friendly name for the mitigation action.
    name :: Prelude.Maybe Prelude.Text,
    -- | The IAM role ARN used to apply this mitigation action.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MitigationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionParams', 'mitigationAction_actionParams' - The set of parameters for this mitigation action. The parameters vary,
-- depending on the kind of action you apply.
--
-- 'id', 'mitigationAction_id' - A unique identifier for the mitigation action.
--
-- 'name', 'mitigationAction_name' - A user-friendly name for the mitigation action.
--
-- 'roleArn', 'mitigationAction_roleArn' - The IAM role ARN used to apply this mitigation action.
newMitigationAction ::
  MitigationAction
newMitigationAction =
  MitigationAction'
    { actionParams = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The set of parameters for this mitigation action. The parameters vary,
-- depending on the kind of action you apply.
mitigationAction_actionParams :: Lens.Lens' MitigationAction (Prelude.Maybe MitigationActionParams)
mitigationAction_actionParams = Lens.lens (\MitigationAction' {actionParams} -> actionParams) (\s@MitigationAction' {} a -> s {actionParams = a} :: MitigationAction)

-- | A unique identifier for the mitigation action.
mitigationAction_id :: Lens.Lens' MitigationAction (Prelude.Maybe Prelude.Text)
mitigationAction_id = Lens.lens (\MitigationAction' {id} -> id) (\s@MitigationAction' {} a -> s {id = a} :: MitigationAction)

-- | A user-friendly name for the mitigation action.
mitigationAction_name :: Lens.Lens' MitigationAction (Prelude.Maybe Prelude.Text)
mitigationAction_name = Lens.lens (\MitigationAction' {name} -> name) (\s@MitigationAction' {} a -> s {name = a} :: MitigationAction)

-- | The IAM role ARN used to apply this mitigation action.
mitigationAction_roleArn :: Lens.Lens' MitigationAction (Prelude.Maybe Prelude.Text)
mitigationAction_roleArn = Lens.lens (\MitigationAction' {roleArn} -> roleArn) (\s@MitigationAction' {} a -> s {roleArn = a} :: MitigationAction)

instance Data.FromJSON MitigationAction where
  parseJSON =
    Data.withObject
      "MitigationAction"
      ( \x ->
          MitigationAction'
            Prelude.<$> (x Data..:? "actionParams")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "roleArn")
      )

instance Prelude.Hashable MitigationAction where
  hashWithSalt _salt MitigationAction' {..} =
    _salt
      `Prelude.hashWithSalt` actionParams
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData MitigationAction where
  rnf MitigationAction' {..} =
    Prelude.rnf actionParams `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf roleArn
