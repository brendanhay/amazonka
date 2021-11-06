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
-- Module      : Amazonka.AccessAnalyzer.Types.Substring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Substring where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A reference to a substring of a literal string in a JSON document.
--
-- /See:/ 'newSubstring' smart constructor.
data Substring = Substring'
  { -- | The length of the substring.
    length :: Prelude.Int,
    -- | The start index of the substring, starting from 0.
    start :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Substring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'length', 'substring_length' - The length of the substring.
--
-- 'start', 'substring_start' - The start index of the substring, starting from 0.
newSubstring ::
  -- | 'length'
  Prelude.Int ->
  -- | 'start'
  Prelude.Int ->
  Substring
newSubstring pLength_ pStart_ =
  Substring' {length = pLength_, start = pStart_}

-- | The length of the substring.
substring_length :: Lens.Lens' Substring Prelude.Int
substring_length = Lens.lens (\Substring' {length} -> length) (\s@Substring' {} a -> s {length = a} :: Substring)

-- | The start index of the substring, starting from 0.
substring_start :: Lens.Lens' Substring Prelude.Int
substring_start = Lens.lens (\Substring' {start} -> start) (\s@Substring' {} a -> s {start = a} :: Substring)

instance Core.FromJSON Substring where
  parseJSON =
    Core.withObject
      "Substring"
      ( \x ->
          Substring'
            Prelude.<$> (x Core..: "length") Prelude.<*> (x Core..: "start")
      )

instance Prelude.Hashable Substring

instance Prelude.NFData Substring
