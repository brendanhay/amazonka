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
-- Module      : Amazonka.Inspector2.Types.CodeSnippetResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CodeSnippetResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CodeLine
import Amazonka.Inspector2.Types.SuggestedFix
import qualified Amazonka.Prelude as Prelude

-- | Contains information on a code snippet retrieved by Amazon Inspector
-- from a code vulnerability finding.
--
-- /See:/ 'newCodeSnippetResult' smart constructor.
data CodeSnippetResult = CodeSnippetResult'
  { -- | Contains information on the retrieved code snippet.
    codeSnippet :: Prelude.Maybe (Prelude.NonEmpty CodeLine),
    -- | The line number of the last line of a code snippet.
    endLine :: Prelude.Maybe Prelude.Int,
    -- | The ARN of a finding that the code snippet is associated with.
    findingArn :: Prelude.Maybe Prelude.Text,
    -- | The line number of the first line of a code snippet.
    startLine :: Prelude.Maybe Prelude.Int,
    -- | Details of a suggested code fix.
    suggestedFixes :: Prelude.Maybe (Prelude.NonEmpty SuggestedFix)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeSnippetResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSnippet', 'codeSnippetResult_codeSnippet' - Contains information on the retrieved code snippet.
--
-- 'endLine', 'codeSnippetResult_endLine' - The line number of the last line of a code snippet.
--
-- 'findingArn', 'codeSnippetResult_findingArn' - The ARN of a finding that the code snippet is associated with.
--
-- 'startLine', 'codeSnippetResult_startLine' - The line number of the first line of a code snippet.
--
-- 'suggestedFixes', 'codeSnippetResult_suggestedFixes' - Details of a suggested code fix.
newCodeSnippetResult ::
  CodeSnippetResult
newCodeSnippetResult =
  CodeSnippetResult'
    { codeSnippet = Prelude.Nothing,
      endLine = Prelude.Nothing,
      findingArn = Prelude.Nothing,
      startLine = Prelude.Nothing,
      suggestedFixes = Prelude.Nothing
    }

-- | Contains information on the retrieved code snippet.
codeSnippetResult_codeSnippet :: Lens.Lens' CodeSnippetResult (Prelude.Maybe (Prelude.NonEmpty CodeLine))
codeSnippetResult_codeSnippet = Lens.lens (\CodeSnippetResult' {codeSnippet} -> codeSnippet) (\s@CodeSnippetResult' {} a -> s {codeSnippet = a} :: CodeSnippetResult) Prelude.. Lens.mapping Lens.coerced

-- | The line number of the last line of a code snippet.
codeSnippetResult_endLine :: Lens.Lens' CodeSnippetResult (Prelude.Maybe Prelude.Int)
codeSnippetResult_endLine = Lens.lens (\CodeSnippetResult' {endLine} -> endLine) (\s@CodeSnippetResult' {} a -> s {endLine = a} :: CodeSnippetResult)

-- | The ARN of a finding that the code snippet is associated with.
codeSnippetResult_findingArn :: Lens.Lens' CodeSnippetResult (Prelude.Maybe Prelude.Text)
codeSnippetResult_findingArn = Lens.lens (\CodeSnippetResult' {findingArn} -> findingArn) (\s@CodeSnippetResult' {} a -> s {findingArn = a} :: CodeSnippetResult)

-- | The line number of the first line of a code snippet.
codeSnippetResult_startLine :: Lens.Lens' CodeSnippetResult (Prelude.Maybe Prelude.Int)
codeSnippetResult_startLine = Lens.lens (\CodeSnippetResult' {startLine} -> startLine) (\s@CodeSnippetResult' {} a -> s {startLine = a} :: CodeSnippetResult)

-- | Details of a suggested code fix.
codeSnippetResult_suggestedFixes :: Lens.Lens' CodeSnippetResult (Prelude.Maybe (Prelude.NonEmpty SuggestedFix))
codeSnippetResult_suggestedFixes = Lens.lens (\CodeSnippetResult' {suggestedFixes} -> suggestedFixes) (\s@CodeSnippetResult' {} a -> s {suggestedFixes = a} :: CodeSnippetResult) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CodeSnippetResult where
  parseJSON =
    Data.withObject
      "CodeSnippetResult"
      ( \x ->
          CodeSnippetResult'
            Prelude.<$> (x Data..:? "codeSnippet")
            Prelude.<*> (x Data..:? "endLine")
            Prelude.<*> (x Data..:? "findingArn")
            Prelude.<*> (x Data..:? "startLine")
            Prelude.<*> (x Data..:? "suggestedFixes")
      )

instance Prelude.Hashable CodeSnippetResult where
  hashWithSalt _salt CodeSnippetResult' {..} =
    _salt
      `Prelude.hashWithSalt` codeSnippet
      `Prelude.hashWithSalt` endLine
      `Prelude.hashWithSalt` findingArn
      `Prelude.hashWithSalt` startLine
      `Prelude.hashWithSalt` suggestedFixes

instance Prelude.NFData CodeSnippetResult where
  rnf CodeSnippetResult' {..} =
    Prelude.rnf codeSnippet
      `Prelude.seq` Prelude.rnf endLine
      `Prelude.seq` Prelude.rnf findingArn
      `Prelude.seq` Prelude.rnf startLine
      `Prelude.seq` Prelude.rnf suggestedFixes
