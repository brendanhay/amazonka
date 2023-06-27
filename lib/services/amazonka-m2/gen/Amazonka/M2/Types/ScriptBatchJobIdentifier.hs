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
-- Module      : Amazonka.M2.Types.ScriptBatchJobIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.ScriptBatchJobIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A batch job identifier in which the batch job to run is identified by
-- the script name.
--
-- /See:/ 'newScriptBatchJobIdentifier' smart constructor.
data ScriptBatchJobIdentifier = ScriptBatchJobIdentifier'
  { -- | The name of the script containing the batch job definition.
    scriptName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScriptBatchJobIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scriptName', 'scriptBatchJobIdentifier_scriptName' - The name of the script containing the batch job definition.
newScriptBatchJobIdentifier ::
  -- | 'scriptName'
  Prelude.Text ->
  ScriptBatchJobIdentifier
newScriptBatchJobIdentifier pScriptName_ =
  ScriptBatchJobIdentifier'
    { scriptName =
        pScriptName_
    }

-- | The name of the script containing the batch job definition.
scriptBatchJobIdentifier_scriptName :: Lens.Lens' ScriptBatchJobIdentifier Prelude.Text
scriptBatchJobIdentifier_scriptName = Lens.lens (\ScriptBatchJobIdentifier' {scriptName} -> scriptName) (\s@ScriptBatchJobIdentifier' {} a -> s {scriptName = a} :: ScriptBatchJobIdentifier)

instance Data.FromJSON ScriptBatchJobIdentifier where
  parseJSON =
    Data.withObject
      "ScriptBatchJobIdentifier"
      ( \x ->
          ScriptBatchJobIdentifier'
            Prelude.<$> (x Data..: "scriptName")
      )

instance Prelude.Hashable ScriptBatchJobIdentifier where
  hashWithSalt _salt ScriptBatchJobIdentifier' {..} =
    _salt `Prelude.hashWithSalt` scriptName

instance Prelude.NFData ScriptBatchJobIdentifier where
  rnf ScriptBatchJobIdentifier' {..} =
    Prelude.rnf scriptName

instance Data.ToJSON ScriptBatchJobIdentifier where
  toJSON ScriptBatchJobIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("scriptName" Data..= scriptName)]
      )
