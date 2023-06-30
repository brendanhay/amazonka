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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRouteHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRouteHeader where

import Amazonka.AppMesh.Types.HeaderMatchMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the HTTP header in the gateway route.
--
-- /See:/ 'newHttpGatewayRouteHeader' smart constructor.
data HttpGatewayRouteHeader = HttpGatewayRouteHeader'
  { -- | Specify @True@ to match anything except the match criteria. The default
    -- value is @False@.
    invert :: Prelude.Maybe Prelude.Bool,
    -- | An object that represents the method and value to match with the header
    -- value sent in a request. Specify one match method.
    match :: Prelude.Maybe HeaderMatchMethod,
    -- | A name for the HTTP header in the gateway route that will be matched on.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRouteHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invert', 'httpGatewayRouteHeader_invert' - Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
--
-- 'match', 'httpGatewayRouteHeader_match' - An object that represents the method and value to match with the header
-- value sent in a request. Specify one match method.
--
-- 'name', 'httpGatewayRouteHeader_name' - A name for the HTTP header in the gateway route that will be matched on.
newHttpGatewayRouteHeader ::
  -- | 'name'
  Prelude.Text ->
  HttpGatewayRouteHeader
newHttpGatewayRouteHeader pName_ =
  HttpGatewayRouteHeader'
    { invert = Prelude.Nothing,
      match = Prelude.Nothing,
      name = pName_
    }

-- | Specify @True@ to match anything except the match criteria. The default
-- value is @False@.
httpGatewayRouteHeader_invert :: Lens.Lens' HttpGatewayRouteHeader (Prelude.Maybe Prelude.Bool)
httpGatewayRouteHeader_invert = Lens.lens (\HttpGatewayRouteHeader' {invert} -> invert) (\s@HttpGatewayRouteHeader' {} a -> s {invert = a} :: HttpGatewayRouteHeader)

-- | An object that represents the method and value to match with the header
-- value sent in a request. Specify one match method.
httpGatewayRouteHeader_match :: Lens.Lens' HttpGatewayRouteHeader (Prelude.Maybe HeaderMatchMethod)
httpGatewayRouteHeader_match = Lens.lens (\HttpGatewayRouteHeader' {match} -> match) (\s@HttpGatewayRouteHeader' {} a -> s {match = a} :: HttpGatewayRouteHeader)

-- | A name for the HTTP header in the gateway route that will be matched on.
httpGatewayRouteHeader_name :: Lens.Lens' HttpGatewayRouteHeader Prelude.Text
httpGatewayRouteHeader_name = Lens.lens (\HttpGatewayRouteHeader' {name} -> name) (\s@HttpGatewayRouteHeader' {} a -> s {name = a} :: HttpGatewayRouteHeader)

instance Data.FromJSON HttpGatewayRouteHeader where
  parseJSON =
    Data.withObject
      "HttpGatewayRouteHeader"
      ( \x ->
          HttpGatewayRouteHeader'
            Prelude.<$> (x Data..:? "invert")
            Prelude.<*> (x Data..:? "match")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable HttpGatewayRouteHeader where
  hashWithSalt _salt HttpGatewayRouteHeader' {..} =
    _salt
      `Prelude.hashWithSalt` invert
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` name

instance Prelude.NFData HttpGatewayRouteHeader where
  rnf HttpGatewayRouteHeader' {..} =
    Prelude.rnf invert
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON HttpGatewayRouteHeader where
  toJSON HttpGatewayRouteHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("invert" Data..=) Prelude.<$> invert,
            ("match" Data..=) Prelude.<$> match,
            Prelude.Just ("name" Data..= name)
          ]
      )
