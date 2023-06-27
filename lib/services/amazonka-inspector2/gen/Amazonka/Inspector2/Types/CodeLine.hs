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
-- Module      : Amazonka.Inspector2.Types.CodeLine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CodeLine where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the lines of code associated with a code
-- snippet.
--
-- /See:/ 'newCodeLine' smart constructor.
data CodeLine = CodeLine'
  { -- | The content of a line of code
    content :: Prelude.Text,
    -- | The line number that a section of code is located at.
    lineNumber :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeLine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'codeLine_content' - The content of a line of code
--
-- 'lineNumber', 'codeLine_lineNumber' - The line number that a section of code is located at.
newCodeLine ::
  -- | 'content'
  Prelude.Text ->
  -- | 'lineNumber'
  Prelude.Int ->
  CodeLine
newCodeLine pContent_ pLineNumber_ =
  CodeLine'
    { content = pContent_,
      lineNumber = pLineNumber_
    }

-- | The content of a line of code
codeLine_content :: Lens.Lens' CodeLine Prelude.Text
codeLine_content = Lens.lens (\CodeLine' {content} -> content) (\s@CodeLine' {} a -> s {content = a} :: CodeLine)

-- | The line number that a section of code is located at.
codeLine_lineNumber :: Lens.Lens' CodeLine Prelude.Int
codeLine_lineNumber = Lens.lens (\CodeLine' {lineNumber} -> lineNumber) (\s@CodeLine' {} a -> s {lineNumber = a} :: CodeLine)

instance Data.FromJSON CodeLine where
  parseJSON =
    Data.withObject
      "CodeLine"
      ( \x ->
          CodeLine'
            Prelude.<$> (x Data..: "content")
            Prelude.<*> (x Data..: "lineNumber")
      )

instance Prelude.Hashable CodeLine where
  hashWithSalt _salt CodeLine' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` lineNumber

instance Prelude.NFData CodeLine where
  rnf CodeLine' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf lineNumber
