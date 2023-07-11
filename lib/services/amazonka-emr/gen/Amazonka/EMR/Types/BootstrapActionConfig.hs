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
-- Module      : Amazonka.EMR.Types.BootstrapActionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.BootstrapActionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.ScriptBootstrapActionConfig
import qualified Amazonka.Prelude as Prelude

-- | Configuration of a bootstrap action.
--
-- /See:/ 'newBootstrapActionConfig' smart constructor.
data BootstrapActionConfig = BootstrapActionConfig'
  { -- | The name of the bootstrap action.
    name :: Prelude.Text,
    -- | The script run by the bootstrap action.
    scriptBootstrapAction :: ScriptBootstrapActionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BootstrapActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'bootstrapActionConfig_name' - The name of the bootstrap action.
--
-- 'scriptBootstrapAction', 'bootstrapActionConfig_scriptBootstrapAction' - The script run by the bootstrap action.
newBootstrapActionConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scriptBootstrapAction'
  ScriptBootstrapActionConfig ->
  BootstrapActionConfig
newBootstrapActionConfig
  pName_
  pScriptBootstrapAction_ =
    BootstrapActionConfig'
      { name = pName_,
        scriptBootstrapAction = pScriptBootstrapAction_
      }

-- | The name of the bootstrap action.
bootstrapActionConfig_name :: Lens.Lens' BootstrapActionConfig Prelude.Text
bootstrapActionConfig_name = Lens.lens (\BootstrapActionConfig' {name} -> name) (\s@BootstrapActionConfig' {} a -> s {name = a} :: BootstrapActionConfig)

-- | The script run by the bootstrap action.
bootstrapActionConfig_scriptBootstrapAction :: Lens.Lens' BootstrapActionConfig ScriptBootstrapActionConfig
bootstrapActionConfig_scriptBootstrapAction = Lens.lens (\BootstrapActionConfig' {scriptBootstrapAction} -> scriptBootstrapAction) (\s@BootstrapActionConfig' {} a -> s {scriptBootstrapAction = a} :: BootstrapActionConfig)

instance Prelude.Hashable BootstrapActionConfig where
  hashWithSalt _salt BootstrapActionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scriptBootstrapAction

instance Prelude.NFData BootstrapActionConfig where
  rnf BootstrapActionConfig' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf scriptBootstrapAction

instance Data.ToJSON BootstrapActionConfig where
  toJSON BootstrapActionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "ScriptBootstrapAction"
                  Data..= scriptBootstrapAction
              )
          ]
      )
