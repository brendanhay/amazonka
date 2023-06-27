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
-- Module      : Amazonka.VPCLattice.Types.Matcher
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.Matcher where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The codes to use when checking for a successful response from a target
-- for health checks.
--
-- /See:/ 'newMatcher' smart constructor.
data Matcher = Matcher'
  { -- | The HTTP code to use when checking for a successful response from a
    -- target.
    httpCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Matcher' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpCode', 'matcher_httpCode' - The HTTP code to use when checking for a successful response from a
-- target.
newMatcher ::
  Matcher
newMatcher = Matcher' {httpCode = Prelude.Nothing}

-- | The HTTP code to use when checking for a successful response from a
-- target.
matcher_httpCode :: Lens.Lens' Matcher (Prelude.Maybe Prelude.Text)
matcher_httpCode = Lens.lens (\Matcher' {httpCode} -> httpCode) (\s@Matcher' {} a -> s {httpCode = a} :: Matcher)

instance Data.FromJSON Matcher where
  parseJSON =
    Data.withObject
      "Matcher"
      (\x -> Matcher' Prelude.<$> (x Data..:? "httpCode"))

instance Prelude.Hashable Matcher where
  hashWithSalt _salt Matcher' {..} =
    _salt `Prelude.hashWithSalt` httpCode

instance Prelude.NFData Matcher where
  rnf Matcher' {..} = Prelude.rnf httpCode

instance Data.ToJSON Matcher where
  toJSON Matcher' {..} =
    Data.object
      ( Prelude.catMaybes
          [("httpCode" Data..=) Prelude.<$> httpCode]
      )
