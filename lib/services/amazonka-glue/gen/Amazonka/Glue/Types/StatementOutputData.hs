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
-- Module      : Amazonka.Glue.Types.StatementOutputData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StatementOutputData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The code execution output in JSON format.
--
-- /See:/ 'newStatementOutputData' smart constructor.
data StatementOutputData = StatementOutputData'
  { -- | The code execution output in text format.
    textPlain :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatementOutputData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textPlain', 'statementOutputData_textPlain' - The code execution output in text format.
newStatementOutputData ::
  StatementOutputData
newStatementOutputData =
  StatementOutputData' {textPlain = Prelude.Nothing}

-- | The code execution output in text format.
statementOutputData_textPlain :: Lens.Lens' StatementOutputData (Prelude.Maybe Prelude.Text)
statementOutputData_textPlain = Lens.lens (\StatementOutputData' {textPlain} -> textPlain) (\s@StatementOutputData' {} a -> s {textPlain = a} :: StatementOutputData)

instance Data.FromJSON StatementOutputData where
  parseJSON =
    Data.withObject
      "StatementOutputData"
      ( \x ->
          StatementOutputData'
            Prelude.<$> (x Data..:? "TextPlain")
      )

instance Prelude.Hashable StatementOutputData where
  hashWithSalt _salt StatementOutputData' {..} =
    _salt `Prelude.hashWithSalt` textPlain

instance Prelude.NFData StatementOutputData where
  rnf StatementOutputData' {..} = Prelude.rnf textPlain
