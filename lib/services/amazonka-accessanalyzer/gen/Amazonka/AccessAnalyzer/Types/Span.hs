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
-- Module      : Amazonka.AccessAnalyzer.Types.Span
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Span where

import Amazonka.AccessAnalyzer.Types.Position
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A span in a policy. The span consists of a start position (inclusive)
-- and end position (exclusive).
--
-- /See:/ 'newSpan' smart constructor.
data Span = Span'
  { -- | The start position of the span (inclusive).
    start :: Position,
    -- | The end position of the span (exclusive).
    end :: Position
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Span' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'start', 'span_start' - The start position of the span (inclusive).
--
-- 'end', 'span_end' - The end position of the span (exclusive).
newSpan ::
  -- | 'start'
  Position ->
  -- | 'end'
  Position ->
  Span
newSpan pStart_ pEnd_ =
  Span' {start = pStart_, end = pEnd_}

-- | The start position of the span (inclusive).
span_start :: Lens.Lens' Span Position
span_start = Lens.lens (\Span' {start} -> start) (\s@Span' {} a -> s {start = a} :: Span)

-- | The end position of the span (exclusive).
span_end :: Lens.Lens' Span Position
span_end = Lens.lens (\Span' {end} -> end) (\s@Span' {} a -> s {end = a} :: Span)

instance Data.FromJSON Span where
  parseJSON =
    Data.withObject
      "Span"
      ( \x ->
          Span'
            Prelude.<$> (x Data..: "start") Prelude.<*> (x Data..: "end")
      )

instance Prelude.Hashable Span where
  hashWithSalt _salt Span' {..} =
    _salt `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` end

instance Prelude.NFData Span where
  rnf Span' {..} =
    Prelude.rnf start `Prelude.seq` Prelude.rnf end
