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
-- Module      : Amazonka.Glue.Types.SessionCommand
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SessionCommand where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @SessionCommand@ that runs the job.
--
-- /See:/ 'newSessionCommand' smart constructor.
data SessionCommand = SessionCommand'
  { -- | Specifies the name of the SessionCommand. Can be \'glueetl\' or
    -- \'gluestreaming\'.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Python version. The Python version indicates the version
    -- supported for jobs of type Spark.
    pythonVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'sessionCommand_name' - Specifies the name of the SessionCommand. Can be \'glueetl\' or
-- \'gluestreaming\'.
--
-- 'pythonVersion', 'sessionCommand_pythonVersion' - Specifies the Python version. The Python version indicates the version
-- supported for jobs of type Spark.
newSessionCommand ::
  SessionCommand
newSessionCommand =
  SessionCommand'
    { name = Prelude.Nothing,
      pythonVersion = Prelude.Nothing
    }

-- | Specifies the name of the SessionCommand. Can be \'glueetl\' or
-- \'gluestreaming\'.
sessionCommand_name :: Lens.Lens' SessionCommand (Prelude.Maybe Prelude.Text)
sessionCommand_name = Lens.lens (\SessionCommand' {name} -> name) (\s@SessionCommand' {} a -> s {name = a} :: SessionCommand)

-- | Specifies the Python version. The Python version indicates the version
-- supported for jobs of type Spark.
sessionCommand_pythonVersion :: Lens.Lens' SessionCommand (Prelude.Maybe Prelude.Text)
sessionCommand_pythonVersion = Lens.lens (\SessionCommand' {pythonVersion} -> pythonVersion) (\s@SessionCommand' {} a -> s {pythonVersion = a} :: SessionCommand)

instance Data.FromJSON SessionCommand where
  parseJSON =
    Data.withObject
      "SessionCommand"
      ( \x ->
          SessionCommand'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PythonVersion")
      )

instance Prelude.Hashable SessionCommand where
  hashWithSalt _salt SessionCommand' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pythonVersion

instance Prelude.NFData SessionCommand where
  rnf SessionCommand' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf pythonVersion

instance Data.ToJSON SessionCommand where
  toJSON SessionCommand' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("PythonVersion" Data..=) Prelude.<$> pythonVersion
          ]
      )
