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
-- Module      : Amazonka.AppMesh.Types.HttpPathMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpPathMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the path to match in the request.
--
-- /See:/ 'newHttpPathMatch' smart constructor.
data HttpPathMatch = HttpPathMatch'
  { -- | The regex used to match the path.
    regex :: Prelude.Maybe Prelude.Text,
    -- | The exact path to match on.
    exact :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpPathMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regex', 'httpPathMatch_regex' - The regex used to match the path.
--
-- 'exact', 'httpPathMatch_exact' - The exact path to match on.
newHttpPathMatch ::
  HttpPathMatch
newHttpPathMatch =
  HttpPathMatch'
    { regex = Prelude.Nothing,
      exact = Prelude.Nothing
    }

-- | The regex used to match the path.
httpPathMatch_regex :: Lens.Lens' HttpPathMatch (Prelude.Maybe Prelude.Text)
httpPathMatch_regex = Lens.lens (\HttpPathMatch' {regex} -> regex) (\s@HttpPathMatch' {} a -> s {regex = a} :: HttpPathMatch)

-- | The exact path to match on.
httpPathMatch_exact :: Lens.Lens' HttpPathMatch (Prelude.Maybe Prelude.Text)
httpPathMatch_exact = Lens.lens (\HttpPathMatch' {exact} -> exact) (\s@HttpPathMatch' {} a -> s {exact = a} :: HttpPathMatch)

instance Core.FromJSON HttpPathMatch where
  parseJSON =
    Core.withObject
      "HttpPathMatch"
      ( \x ->
          HttpPathMatch'
            Prelude.<$> (x Core..:? "regex")
            Prelude.<*> (x Core..:? "exact")
      )

instance Prelude.Hashable HttpPathMatch where
  hashWithSalt salt' HttpPathMatch' {..} =
    salt' `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` regex

instance Prelude.NFData HttpPathMatch where
  rnf HttpPathMatch' {..} =
    Prelude.rnf regex `Prelude.seq` Prelude.rnf exact

instance Core.ToJSON HttpPathMatch where
  toJSON HttpPathMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("regex" Core..=) Prelude.<$> regex,
            ("exact" Core..=) Prelude.<$> exact
          ]
      )
