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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpPathMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the path to match in the request.
--
-- /See:/ 'newHttpPathMatch' smart constructor.
data HttpPathMatch = HttpPathMatch'
  { -- | The exact path to match on.
    exact :: Prelude.Maybe Prelude.Text,
    -- | The regex used to match the path.
    regex :: Prelude.Maybe Prelude.Text
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
-- 'exact', 'httpPathMatch_exact' - The exact path to match on.
--
-- 'regex', 'httpPathMatch_regex' - The regex used to match the path.
newHttpPathMatch ::
  HttpPathMatch
newHttpPathMatch =
  HttpPathMatch'
    { exact = Prelude.Nothing,
      regex = Prelude.Nothing
    }

-- | The exact path to match on.
httpPathMatch_exact :: Lens.Lens' HttpPathMatch (Prelude.Maybe Prelude.Text)
httpPathMatch_exact = Lens.lens (\HttpPathMatch' {exact} -> exact) (\s@HttpPathMatch' {} a -> s {exact = a} :: HttpPathMatch)

-- | The regex used to match the path.
httpPathMatch_regex :: Lens.Lens' HttpPathMatch (Prelude.Maybe Prelude.Text)
httpPathMatch_regex = Lens.lens (\HttpPathMatch' {regex} -> regex) (\s@HttpPathMatch' {} a -> s {regex = a} :: HttpPathMatch)

instance Data.FromJSON HttpPathMatch where
  parseJSON =
    Data.withObject
      "HttpPathMatch"
      ( \x ->
          HttpPathMatch'
            Prelude.<$> (x Data..:? "exact")
            Prelude.<*> (x Data..:? "regex")
      )

instance Prelude.Hashable HttpPathMatch where
  hashWithSalt _salt HttpPathMatch' {..} =
    _salt
      `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` regex

instance Prelude.NFData HttpPathMatch where
  rnf HttpPathMatch' {..} =
    Prelude.rnf exact `Prelude.seq` Prelude.rnf regex

instance Data.ToJSON HttpPathMatch where
  toJSON HttpPathMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exact" Data..=) Prelude.<$> exact,
            ("regex" Data..=) Prelude.<$> regex
          ]
      )
