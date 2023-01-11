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
-- Module      : Amazonka.EMR.Types.ScriptBootstrapActionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ScriptBootstrapActionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration of the script to run during a bootstrap action.
--
-- /See:/ 'newScriptBootstrapActionConfig' smart constructor.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig'
  { -- | A list of command line arguments to pass to the bootstrap action script.
    args :: Prelude.Maybe [Prelude.Text],
    -- | Location in Amazon S3 of the script to run during a bootstrap action.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScriptBootstrapActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'scriptBootstrapActionConfig_args' - A list of command line arguments to pass to the bootstrap action script.
--
-- 'path', 'scriptBootstrapActionConfig_path' - Location in Amazon S3 of the script to run during a bootstrap action.
newScriptBootstrapActionConfig ::
  -- | 'path'
  Prelude.Text ->
  ScriptBootstrapActionConfig
newScriptBootstrapActionConfig pPath_ =
  ScriptBootstrapActionConfig'
    { args =
        Prelude.Nothing,
      path = pPath_
    }

-- | A list of command line arguments to pass to the bootstrap action script.
scriptBootstrapActionConfig_args :: Lens.Lens' ScriptBootstrapActionConfig (Prelude.Maybe [Prelude.Text])
scriptBootstrapActionConfig_args = Lens.lens (\ScriptBootstrapActionConfig' {args} -> args) (\s@ScriptBootstrapActionConfig' {} a -> s {args = a} :: ScriptBootstrapActionConfig) Prelude.. Lens.mapping Lens.coerced

-- | Location in Amazon S3 of the script to run during a bootstrap action.
scriptBootstrapActionConfig_path :: Lens.Lens' ScriptBootstrapActionConfig Prelude.Text
scriptBootstrapActionConfig_path = Lens.lens (\ScriptBootstrapActionConfig' {path} -> path) (\s@ScriptBootstrapActionConfig' {} a -> s {path = a} :: ScriptBootstrapActionConfig)

instance Prelude.Hashable ScriptBootstrapActionConfig where
  hashWithSalt _salt ScriptBootstrapActionConfig' {..} =
    _salt `Prelude.hashWithSalt` args
      `Prelude.hashWithSalt` path

instance Prelude.NFData ScriptBootstrapActionConfig where
  rnf ScriptBootstrapActionConfig' {..} =
    Prelude.rnf args `Prelude.seq` Prelude.rnf path

instance Data.ToJSON ScriptBootstrapActionConfig where
  toJSON ScriptBootstrapActionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Args" Data..=) Prelude.<$> args,
            Prelude.Just ("Path" Data..= path)
          ]
      )
