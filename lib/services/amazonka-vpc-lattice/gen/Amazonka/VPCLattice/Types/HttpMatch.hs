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
-- Module      : Amazonka.VPCLattice.Types.HttpMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.HttpMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.HeaderMatch
import Amazonka.VPCLattice.Types.PathMatch

-- | Describes criteria that can be applied to incoming requests.
--
-- /See:/ 'newHttpMatch' smart constructor.
data HttpMatch = HttpMatch'
  { -- | The header matches. Matches incoming requests with rule based on request
    -- header value before applying rule action.
    headerMatches :: Prelude.Maybe (Prelude.NonEmpty HeaderMatch),
    -- | The HTTP method type.
    method :: Prelude.Maybe Prelude.Text,
    -- | The path match.
    pathMatch :: Prelude.Maybe PathMatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerMatches', 'httpMatch_headerMatches' - The header matches. Matches incoming requests with rule based on request
-- header value before applying rule action.
--
-- 'method', 'httpMatch_method' - The HTTP method type.
--
-- 'pathMatch', 'httpMatch_pathMatch' - The path match.
newHttpMatch ::
  HttpMatch
newHttpMatch =
  HttpMatch'
    { headerMatches = Prelude.Nothing,
      method = Prelude.Nothing,
      pathMatch = Prelude.Nothing
    }

-- | The header matches. Matches incoming requests with rule based on request
-- header value before applying rule action.
httpMatch_headerMatches :: Lens.Lens' HttpMatch (Prelude.Maybe (Prelude.NonEmpty HeaderMatch))
httpMatch_headerMatches = Lens.lens (\HttpMatch' {headerMatches} -> headerMatches) (\s@HttpMatch' {} a -> s {headerMatches = a} :: HttpMatch) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP method type.
httpMatch_method :: Lens.Lens' HttpMatch (Prelude.Maybe Prelude.Text)
httpMatch_method = Lens.lens (\HttpMatch' {method} -> method) (\s@HttpMatch' {} a -> s {method = a} :: HttpMatch)

-- | The path match.
httpMatch_pathMatch :: Lens.Lens' HttpMatch (Prelude.Maybe PathMatch)
httpMatch_pathMatch = Lens.lens (\HttpMatch' {pathMatch} -> pathMatch) (\s@HttpMatch' {} a -> s {pathMatch = a} :: HttpMatch)

instance Data.FromJSON HttpMatch where
  parseJSON =
    Data.withObject
      "HttpMatch"
      ( \x ->
          HttpMatch'
            Prelude.<$> (x Data..:? "headerMatches")
            Prelude.<*> (x Data..:? "method")
            Prelude.<*> (x Data..:? "pathMatch")
      )

instance Prelude.Hashable HttpMatch where
  hashWithSalt _salt HttpMatch' {..} =
    _salt
      `Prelude.hashWithSalt` headerMatches
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` pathMatch

instance Prelude.NFData HttpMatch where
  rnf HttpMatch' {..} =
    Prelude.rnf headerMatches
      `Prelude.seq` Prelude.rnf method
      `Prelude.seq` Prelude.rnf pathMatch

instance Data.ToJSON HttpMatch where
  toJSON HttpMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("headerMatches" Data..=) Prelude.<$> headerMatches,
            ("method" Data..=) Prelude.<$> method,
            ("pathMatch" Data..=) Prelude.<$> pathMatch
          ]
      )
