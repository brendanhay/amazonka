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
-- Module      : Network.AWS.AppMesh.Types.HttpQueryParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.HttpQueryParameter where

import Network.AWS.AppMesh.Types.QueryParameterMatch
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON HttpQueryParameter where
  parseJSON =
    Core.withObject
      "HttpQueryParameter"
      ( \x ->
          HttpQueryParameter'
            Prelude.<$> (x Core..:? "match") Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable HttpQueryParameter

instance Prelude.NFData HttpQueryParameter

instance Core.ToJSON HttpQueryParameter where
  toJSON HttpQueryParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("match" Core..=) Prelude.<$> match,
            Prelude.Just ("name" Core..= name)
          ]
      )
