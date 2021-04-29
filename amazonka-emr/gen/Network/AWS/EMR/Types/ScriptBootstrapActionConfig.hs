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
-- Module      : Network.AWS.EMR.Types.ScriptBootstrapActionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScriptBootstrapActionConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration of the script to run during a bootstrap action.
--
-- /See:/ 'newScriptBootstrapActionConfig' smart constructor.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig'
  { -- | A list of command line arguments to pass to the bootstrap action script.
    args :: Prelude.Maybe [Prelude.Text],
    -- | Location of the script to run during a bootstrap action. Can be either a
    -- location in Amazon S3 or on a local file system.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'path', 'scriptBootstrapActionConfig_path' - Location of the script to run during a bootstrap action. Can be either a
-- location in Amazon S3 or on a local file system.
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
scriptBootstrapActionConfig_args = Lens.lens (\ScriptBootstrapActionConfig' {args} -> args) (\s@ScriptBootstrapActionConfig' {} a -> s {args = a} :: ScriptBootstrapActionConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Location of the script to run during a bootstrap action. Can be either a
-- location in Amazon S3 or on a local file system.
scriptBootstrapActionConfig_path :: Lens.Lens' ScriptBootstrapActionConfig Prelude.Text
scriptBootstrapActionConfig_path = Lens.lens (\ScriptBootstrapActionConfig' {path} -> path) (\s@ScriptBootstrapActionConfig' {} a -> s {path = a} :: ScriptBootstrapActionConfig)

instance Prelude.Hashable ScriptBootstrapActionConfig

instance Prelude.NFData ScriptBootstrapActionConfig

instance Prelude.ToJSON ScriptBootstrapActionConfig where
  toJSON ScriptBootstrapActionConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Args" Prelude..=) Prelude.<$> args,
            Prelude.Just ("Path" Prelude..= path)
          ]
      )
