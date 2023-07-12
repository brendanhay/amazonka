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
-- Module      : Amazonka.CodeBuild.Types.ExportedEnvironmentVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ExportedEnvironmentVariable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an exported environment variable.
--
-- Exported environment variables are used in conjunction with CodePipeline
-- to export environment variables from the current build stage to
-- subsequent stages in the pipeline. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/actions-variables.html Working with variables>
-- in the /CodePipeline User Guide/.
--
-- During a build, the value of a variable is available starting with the
-- @install@ phase. It can be updated between the start of the @install@
-- phase and the end of the @post_build@ phase. After the @post_build@
-- phase ends, the value of exported variables cannot change.
--
-- /See:/ 'newExportedEnvironmentVariable' smart constructor.
data ExportedEnvironmentVariable = ExportedEnvironmentVariable'
  { -- | The name of the exported environment variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value assigned to the exported environment variable.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportedEnvironmentVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'exportedEnvironmentVariable_name' - The name of the exported environment variable.
--
-- 'value', 'exportedEnvironmentVariable_value' - The value assigned to the exported environment variable.
newExportedEnvironmentVariable ::
  ExportedEnvironmentVariable
newExportedEnvironmentVariable =
  ExportedEnvironmentVariable'
    { name =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the exported environment variable.
exportedEnvironmentVariable_name :: Lens.Lens' ExportedEnvironmentVariable (Prelude.Maybe Prelude.Text)
exportedEnvironmentVariable_name = Lens.lens (\ExportedEnvironmentVariable' {name} -> name) (\s@ExportedEnvironmentVariable' {} a -> s {name = a} :: ExportedEnvironmentVariable)

-- | The value assigned to the exported environment variable.
exportedEnvironmentVariable_value :: Lens.Lens' ExportedEnvironmentVariable (Prelude.Maybe Prelude.Text)
exportedEnvironmentVariable_value = Lens.lens (\ExportedEnvironmentVariable' {value} -> value) (\s@ExportedEnvironmentVariable' {} a -> s {value = a} :: ExportedEnvironmentVariable)

instance Data.FromJSON ExportedEnvironmentVariable where
  parseJSON =
    Data.withObject
      "ExportedEnvironmentVariable"
      ( \x ->
          ExportedEnvironmentVariable'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ExportedEnvironmentVariable where
  hashWithSalt _salt ExportedEnvironmentVariable' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ExportedEnvironmentVariable where
  rnf ExportedEnvironmentVariable' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
