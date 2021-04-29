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
-- Module      : Network.AWS.EMR.Types.BootstrapActionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BootstrapActionConfig where

import Network.AWS.EMR.Types.ScriptBootstrapActionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration of a bootstrap action.
--
-- /See:/ 'newBootstrapActionConfig' smart constructor.
data BootstrapActionConfig = BootstrapActionConfig'
  { -- | The name of the bootstrap action.
    name :: Prelude.Text,
    -- | The script run by the bootstrap action.
    scriptBootstrapAction :: ScriptBootstrapActionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable BootstrapActionConfig

instance Prelude.NFData BootstrapActionConfig

instance Prelude.ToJSON BootstrapActionConfig where
  toJSON BootstrapActionConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ( "ScriptBootstrapAction"
                  Prelude..= scriptBootstrapAction
              )
          ]
      )
