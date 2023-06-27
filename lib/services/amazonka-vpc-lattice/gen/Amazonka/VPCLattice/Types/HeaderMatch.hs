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
-- Module      : Amazonka.VPCLattice.Types.HeaderMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.HeaderMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.HeaderMatchType

-- | Describes the constraints for a header match. Matches incoming requests
-- with rule based on request header value before applying rule action.
--
-- /See:/ 'newHeaderMatch' smart constructor.
data HeaderMatch = HeaderMatch'
  { -- | Indicates whether the match is case sensitive. Defaults to false.
    caseSensitive :: Prelude.Maybe Prelude.Bool,
    -- | The header match type.
    match :: HeaderMatchType,
    -- | The name of the header.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeaderMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseSensitive', 'headerMatch_caseSensitive' - Indicates whether the match is case sensitive. Defaults to false.
--
-- 'match', 'headerMatch_match' - The header match type.
--
-- 'name', 'headerMatch_name' - The name of the header.
newHeaderMatch ::
  -- | 'match'
  HeaderMatchType ->
  -- | 'name'
  Prelude.Text ->
  HeaderMatch
newHeaderMatch pMatch_ pName_ =
  HeaderMatch'
    { caseSensitive = Prelude.Nothing,
      match = pMatch_,
      name = pName_
    }

-- | Indicates whether the match is case sensitive. Defaults to false.
headerMatch_caseSensitive :: Lens.Lens' HeaderMatch (Prelude.Maybe Prelude.Bool)
headerMatch_caseSensitive = Lens.lens (\HeaderMatch' {caseSensitive} -> caseSensitive) (\s@HeaderMatch' {} a -> s {caseSensitive = a} :: HeaderMatch)

-- | The header match type.
headerMatch_match :: Lens.Lens' HeaderMatch HeaderMatchType
headerMatch_match = Lens.lens (\HeaderMatch' {match} -> match) (\s@HeaderMatch' {} a -> s {match = a} :: HeaderMatch)

-- | The name of the header.
headerMatch_name :: Lens.Lens' HeaderMatch Prelude.Text
headerMatch_name = Lens.lens (\HeaderMatch' {name} -> name) (\s@HeaderMatch' {} a -> s {name = a} :: HeaderMatch)

instance Data.FromJSON HeaderMatch where
  parseJSON =
    Data.withObject
      "HeaderMatch"
      ( \x ->
          HeaderMatch'
            Prelude.<$> (x Data..:? "caseSensitive")
            Prelude.<*> (x Data..: "match")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable HeaderMatch where
  hashWithSalt _salt HeaderMatch' {..} =
    _salt
      `Prelude.hashWithSalt` caseSensitive
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` name

instance Prelude.NFData HeaderMatch where
  rnf HeaderMatch' {..} =
    Prelude.rnf caseSensitive
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON HeaderMatch where
  toJSON HeaderMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("caseSensitive" Data..=) Prelude.<$> caseSensitive,
            Prelude.Just ("match" Data..= match),
            Prelude.Just ("name" Data..= name)
          ]
      )
