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
-- Module      : Amazonka.AppMesh.Types.HttpQueryParameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpQueryParameter where

import Amazonka.AppMesh.Types.QueryParameterMatch
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the query parameter in the request.
--
-- /See:/ 'newHttpQueryParameter' smart constructor.
data HttpQueryParameter = HttpQueryParameter'
  { -- | The query parameter to match on.
    match :: Prelude.Maybe QueryParameterMatch,
    -- | A name for the query parameter that will be matched on.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpQueryParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'match', 'httpQueryParameter_match' - The query parameter to match on.
--
-- 'name', 'httpQueryParameter_name' - A name for the query parameter that will be matched on.
newHttpQueryParameter ::
  -- | 'name'
  Prelude.Text ->
  HttpQueryParameter
newHttpQueryParameter pName_ =
  HttpQueryParameter'
    { match = Prelude.Nothing,
      name = pName_
    }

-- | The query parameter to match on.
httpQueryParameter_match :: Lens.Lens' HttpQueryParameter (Prelude.Maybe QueryParameterMatch)
httpQueryParameter_match = Lens.lens (\HttpQueryParameter' {match} -> match) (\s@HttpQueryParameter' {} a -> s {match = a} :: HttpQueryParameter)

-- | A name for the query parameter that will be matched on.
httpQueryParameter_name :: Lens.Lens' HttpQueryParameter Prelude.Text
httpQueryParameter_name = Lens.lens (\HttpQueryParameter' {name} -> name) (\s@HttpQueryParameter' {} a -> s {name = a} :: HttpQueryParameter)

instance Data.FromJSON HttpQueryParameter where
  parseJSON =
    Data.withObject
      "HttpQueryParameter"
      ( \x ->
          HttpQueryParameter'
            Prelude.<$> (x Data..:? "match") Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable HttpQueryParameter where
  hashWithSalt _salt HttpQueryParameter' {..} =
    _salt `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` name

instance Prelude.NFData HttpQueryParameter where
  rnf HttpQueryParameter' {..} =
    Prelude.rnf match `Prelude.seq` Prelude.rnf name

instance Data.ToJSON HttpQueryParameter where
  toJSON HttpQueryParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("match" Data..=) Prelude.<$> match,
            Prelude.Just ("name" Data..= name)
          ]
      )
