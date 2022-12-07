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
-- Module      : Amazonka.AppMesh.Types.HttpRouteHeader
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpRouteHeader where

import Amazonka.AppMesh.Types.HeaderMatchMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the HTTP header in the request.
--
-- /See:/ 'newHttpRouteHeader' smart constructor.
data HttpRouteHeader = HttpRouteHeader'
  { -- | The @HeaderMatchMethod@ object.
    match :: Prelude.Maybe HeaderMatchMethod,
    -- | Specify @True@ to match anything except the match criteria. The default
    -- value is @False@.
    invert :: Prelude.Maybe Prelude.Bool,
    -- | A name for the HTTP header in the client request that will be matched
    -- on.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpRouteHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'match', 'httpRouteHeader_match' - The @HeaderMatchMethod@ object.
--
-- 'invert', 'httpRouteHeader_invert' - Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
--
-- 'name', 'httpRouteHeader_name' - A name for the HTTP header in the client request that will be matched
-- on.
newHttpRouteHeader ::
  -- | 'name'
  Prelude.Text ->
  HttpRouteHeader
newHttpRouteHeader pName_ =
  HttpRouteHeader'
    { match = Prelude.Nothing,
      invert = Prelude.Nothing,
      name = pName_
    }

-- | The @HeaderMatchMethod@ object.
httpRouteHeader_match :: Lens.Lens' HttpRouteHeader (Prelude.Maybe HeaderMatchMethod)
httpRouteHeader_match = Lens.lens (\HttpRouteHeader' {match} -> match) (\s@HttpRouteHeader' {} a -> s {match = a} :: HttpRouteHeader)

-- | Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
httpRouteHeader_invert :: Lens.Lens' HttpRouteHeader (Prelude.Maybe Prelude.Bool)
httpRouteHeader_invert = Lens.lens (\HttpRouteHeader' {invert} -> invert) (\s@HttpRouteHeader' {} a -> s {invert = a} :: HttpRouteHeader)

-- | A name for the HTTP header in the client request that will be matched
-- on.
httpRouteHeader_name :: Lens.Lens' HttpRouteHeader Prelude.Text
httpRouteHeader_name = Lens.lens (\HttpRouteHeader' {name} -> name) (\s@HttpRouteHeader' {} a -> s {name = a} :: HttpRouteHeader)

instance Data.FromJSON HttpRouteHeader where
  parseJSON =
    Data.withObject
      "HttpRouteHeader"
      ( \x ->
          HttpRouteHeader'
            Prelude.<$> (x Data..:? "match")
            Prelude.<*> (x Data..:? "invert")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable HttpRouteHeader where
  hashWithSalt _salt HttpRouteHeader' {..} =
    _salt `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` invert
      `Prelude.hashWithSalt` name

instance Prelude.NFData HttpRouteHeader where
  rnf HttpRouteHeader' {..} =
    Prelude.rnf match
      `Prelude.seq` Prelude.rnf invert
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON HttpRouteHeader where
  toJSON HttpRouteHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("match" Data..=) Prelude.<$> match,
            ("invert" Data..=) Prelude.<$> invert,
            Prelude.Just ("name" Data..= name)
          ]
      )
