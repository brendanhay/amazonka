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
-- Module      : Network.AWS.AccessAnalyzer.Types.Span
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.Span where

import Network.AWS.AccessAnalyzer.Types.Position
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A span in a policy. The span consists of a start position (inclusive)
-- and end position (exclusive).
--
-- /See:/ 'newSpan' smart constructor.
data Span = Span'
  { -- | The end position of the span (exclusive).
    end :: Position,
    -- | The start position of the span (inclusive).
    start :: Position
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
-- 'end', 'span_end' - The end position of the span (exclusive).
--
-- 'start', 'span_start' - The start position of the span (inclusive).
newSpan ::
  -- | 'end'
  Position ->
  -- | 'start'
  Position ->
  Span
newSpan pEnd_ pStart_ =
  Span' {end = pEnd_, start = pStart_}

-- | The end position of the span (exclusive).
span_end :: Lens.Lens' Span Position
span_end = Lens.lens (\Span' {end} -> end) (\s@Span' {} a -> s {end = a} :: Span)

-- | The start position of the span (inclusive).
span_start :: Lens.Lens' Span Position
span_start = Lens.lens (\Span' {start} -> start) (\s@Span' {} a -> s {start = a} :: Span)

instance Core.FromJSON Span where
  parseJSON =
    Core.withObject
      "Span"
      ( \x ->
          Span'
            Prelude.<$> (x Core..: "end") Prelude.<*> (x Core..: "start")
      )

instance Prelude.Hashable Span

instance Prelude.NFData Span
