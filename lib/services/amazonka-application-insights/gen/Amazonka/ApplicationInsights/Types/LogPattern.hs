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
-- Module      : Amazonka.ApplicationInsights.Types.LogPattern
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.LogPattern where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines the log patterns that belongs to a
-- @LogPatternSet@.
--
-- /See:/ 'newLogPattern' smart constructor.
data LogPattern = LogPattern'
  { -- | A regular expression that defines the log pattern. A log pattern can
    -- contain as many as 50 characters, and it cannot be empty. The pattern
    -- must be DFA compatible. Patterns that utilize forward lookahead or
    -- backreference constructions are not supported.
    pattern' :: Prelude.Maybe Prelude.Text,
    -- | The name of the log pattern. A log pattern name can contain as many as
    -- 50 characters, and it cannot be empty. The characters can be Unicode
    -- letters, digits, or one of the following symbols: period, dash,
    -- underscore.
    patternName :: Prelude.Maybe Prelude.Text,
    -- | The name of the log pattern. A log pattern name can contain as many as
    -- 30 characters, and it cannot be empty. The characters can be Unicode
    -- letters, digits, or one of the following symbols: period, dash,
    -- underscore.
    patternSetName :: Prelude.Maybe Prelude.Text,
    -- | Rank of the log pattern. Must be a value between @1@ and @1,000,000@.
    -- The patterns are sorted by rank, so we recommend that you set your
    -- highest priority patterns with the lowest rank. A pattern of rank @1@
    -- will be the first to get matched to a log line. A pattern of rank
    -- @1,000,000@ will be last to get matched. When you configure custom log
    -- patterns from the console, a @Low@ severity pattern translates to a
    -- @750,000@ rank. A @Medium@ severity pattern translates to a @500,000@
    -- rank. And a @High@ severity pattern translates to a @250,000@ rank. Rank
    -- values less than @1@ or greater than @1,000,000@ are reserved for
    -- AWS-provided patterns.
    rank :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pattern'', 'logPattern_pattern' - A regular expression that defines the log pattern. A log pattern can
-- contain as many as 50 characters, and it cannot be empty. The pattern
-- must be DFA compatible. Patterns that utilize forward lookahead or
-- backreference constructions are not supported.
--
-- 'patternName', 'logPattern_patternName' - The name of the log pattern. A log pattern name can contain as many as
-- 50 characters, and it cannot be empty. The characters can be Unicode
-- letters, digits, or one of the following symbols: period, dash,
-- underscore.
--
-- 'patternSetName', 'logPattern_patternSetName' - The name of the log pattern. A log pattern name can contain as many as
-- 30 characters, and it cannot be empty. The characters can be Unicode
-- letters, digits, or one of the following symbols: period, dash,
-- underscore.
--
-- 'rank', 'logPattern_rank' - Rank of the log pattern. Must be a value between @1@ and @1,000,000@.
-- The patterns are sorted by rank, so we recommend that you set your
-- highest priority patterns with the lowest rank. A pattern of rank @1@
-- will be the first to get matched to a log line. A pattern of rank
-- @1,000,000@ will be last to get matched. When you configure custom log
-- patterns from the console, a @Low@ severity pattern translates to a
-- @750,000@ rank. A @Medium@ severity pattern translates to a @500,000@
-- rank. And a @High@ severity pattern translates to a @250,000@ rank. Rank
-- values less than @1@ or greater than @1,000,000@ are reserved for
-- AWS-provided patterns.
newLogPattern ::
  LogPattern
newLogPattern =
  LogPattern'
    { pattern' = Prelude.Nothing,
      patternName = Prelude.Nothing,
      patternSetName = Prelude.Nothing,
      rank = Prelude.Nothing
    }

-- | A regular expression that defines the log pattern. A log pattern can
-- contain as many as 50 characters, and it cannot be empty. The pattern
-- must be DFA compatible. Patterns that utilize forward lookahead or
-- backreference constructions are not supported.
logPattern_pattern :: Lens.Lens' LogPattern (Prelude.Maybe Prelude.Text)
logPattern_pattern = Lens.lens (\LogPattern' {pattern'} -> pattern') (\s@LogPattern' {} a -> s {pattern' = a} :: LogPattern)

-- | The name of the log pattern. A log pattern name can contain as many as
-- 50 characters, and it cannot be empty. The characters can be Unicode
-- letters, digits, or one of the following symbols: period, dash,
-- underscore.
logPattern_patternName :: Lens.Lens' LogPattern (Prelude.Maybe Prelude.Text)
logPattern_patternName = Lens.lens (\LogPattern' {patternName} -> patternName) (\s@LogPattern' {} a -> s {patternName = a} :: LogPattern)

-- | The name of the log pattern. A log pattern name can contain as many as
-- 30 characters, and it cannot be empty. The characters can be Unicode
-- letters, digits, or one of the following symbols: period, dash,
-- underscore.
logPattern_patternSetName :: Lens.Lens' LogPattern (Prelude.Maybe Prelude.Text)
logPattern_patternSetName = Lens.lens (\LogPattern' {patternSetName} -> patternSetName) (\s@LogPattern' {} a -> s {patternSetName = a} :: LogPattern)

-- | Rank of the log pattern. Must be a value between @1@ and @1,000,000@.
-- The patterns are sorted by rank, so we recommend that you set your
-- highest priority patterns with the lowest rank. A pattern of rank @1@
-- will be the first to get matched to a log line. A pattern of rank
-- @1,000,000@ will be last to get matched. When you configure custom log
-- patterns from the console, a @Low@ severity pattern translates to a
-- @750,000@ rank. A @Medium@ severity pattern translates to a @500,000@
-- rank. And a @High@ severity pattern translates to a @250,000@ rank. Rank
-- values less than @1@ or greater than @1,000,000@ are reserved for
-- AWS-provided patterns.
logPattern_rank :: Lens.Lens' LogPattern (Prelude.Maybe Prelude.Int)
logPattern_rank = Lens.lens (\LogPattern' {rank} -> rank) (\s@LogPattern' {} a -> s {rank = a} :: LogPattern)

instance Data.FromJSON LogPattern where
  parseJSON =
    Data.withObject
      "LogPattern"
      ( \x ->
          LogPattern'
            Prelude.<$> (x Data..:? "Pattern")
            Prelude.<*> (x Data..:? "PatternName")
            Prelude.<*> (x Data..:? "PatternSetName")
            Prelude.<*> (x Data..:? "Rank")
      )

instance Prelude.Hashable LogPattern where
  hashWithSalt _salt LogPattern' {..} =
    _salt
      `Prelude.hashWithSalt` pattern'
      `Prelude.hashWithSalt` patternName
      `Prelude.hashWithSalt` patternSetName
      `Prelude.hashWithSalt` rank

instance Prelude.NFData LogPattern where
  rnf LogPattern' {..} =
    Prelude.rnf pattern'
      `Prelude.seq` Prelude.rnf patternName
      `Prelude.seq` Prelude.rnf patternSetName
      `Prelude.seq` Prelude.rnf rank
