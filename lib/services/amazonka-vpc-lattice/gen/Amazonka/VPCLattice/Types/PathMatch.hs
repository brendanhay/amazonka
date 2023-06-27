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
-- Module      : Amazonka.VPCLattice.Types.PathMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.PathMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.PathMatchType

-- | Describes the conditions that can be applied when matching a path for
-- incoming requests.
--
-- /See:/ 'newPathMatch' smart constructor.
data PathMatch = PathMatch'
  { -- | Indicates whether the match is case sensitive. Defaults to false.
    caseSensitive :: Prelude.Maybe Prelude.Bool,
    -- | The type of path match.
    match :: PathMatchType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseSensitive', 'pathMatch_caseSensitive' - Indicates whether the match is case sensitive. Defaults to false.
--
-- 'match', 'pathMatch_match' - The type of path match.
newPathMatch ::
  -- | 'match'
  PathMatchType ->
  PathMatch
newPathMatch pMatch_ =
  PathMatch'
    { caseSensitive = Prelude.Nothing,
      match = pMatch_
    }

-- | Indicates whether the match is case sensitive. Defaults to false.
pathMatch_caseSensitive :: Lens.Lens' PathMatch (Prelude.Maybe Prelude.Bool)
pathMatch_caseSensitive = Lens.lens (\PathMatch' {caseSensitive} -> caseSensitive) (\s@PathMatch' {} a -> s {caseSensitive = a} :: PathMatch)

-- | The type of path match.
pathMatch_match :: Lens.Lens' PathMatch PathMatchType
pathMatch_match = Lens.lens (\PathMatch' {match} -> match) (\s@PathMatch' {} a -> s {match = a} :: PathMatch)

instance Data.FromJSON PathMatch where
  parseJSON =
    Data.withObject
      "PathMatch"
      ( \x ->
          PathMatch'
            Prelude.<$> (x Data..:? "caseSensitive")
            Prelude.<*> (x Data..: "match")
      )

instance Prelude.Hashable PathMatch where
  hashWithSalt _salt PathMatch' {..} =
    _salt
      `Prelude.hashWithSalt` caseSensitive
      `Prelude.hashWithSalt` match

instance Prelude.NFData PathMatch where
  rnf PathMatch' {..} =
    Prelude.rnf caseSensitive
      `Prelude.seq` Prelude.rnf match

instance Data.ToJSON PathMatch where
  toJSON PathMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("caseSensitive" Data..=) Prelude.<$> caseSensitive,
            Prelude.Just ("match" Data..= match)
          ]
      )
