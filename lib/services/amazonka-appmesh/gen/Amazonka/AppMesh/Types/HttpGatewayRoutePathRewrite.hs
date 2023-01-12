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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRoutePathRewrite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRoutePathRewrite where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the path to rewrite.
--
-- /See:/ 'newHttpGatewayRoutePathRewrite' smart constructor.
data HttpGatewayRoutePathRewrite = HttpGatewayRoutePathRewrite'
  { -- | The exact path to rewrite.
    exact :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRoutePathRewrite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exact', 'httpGatewayRoutePathRewrite_exact' - The exact path to rewrite.
newHttpGatewayRoutePathRewrite ::
  HttpGatewayRoutePathRewrite
newHttpGatewayRoutePathRewrite =
  HttpGatewayRoutePathRewrite'
    { exact =
        Prelude.Nothing
    }

-- | The exact path to rewrite.
httpGatewayRoutePathRewrite_exact :: Lens.Lens' HttpGatewayRoutePathRewrite (Prelude.Maybe Prelude.Text)
httpGatewayRoutePathRewrite_exact = Lens.lens (\HttpGatewayRoutePathRewrite' {exact} -> exact) (\s@HttpGatewayRoutePathRewrite' {} a -> s {exact = a} :: HttpGatewayRoutePathRewrite)

instance Data.FromJSON HttpGatewayRoutePathRewrite where
  parseJSON =
    Data.withObject
      "HttpGatewayRoutePathRewrite"
      ( \x ->
          HttpGatewayRoutePathRewrite'
            Prelude.<$> (x Data..:? "exact")
      )

instance Prelude.Hashable HttpGatewayRoutePathRewrite where
  hashWithSalt _salt HttpGatewayRoutePathRewrite' {..} =
    _salt `Prelude.hashWithSalt` exact

instance Prelude.NFData HttpGatewayRoutePathRewrite where
  rnf HttpGatewayRoutePathRewrite' {..} =
    Prelude.rnf exact

instance Data.ToJSON HttpGatewayRoutePathRewrite where
  toJSON HttpGatewayRoutePathRewrite' {..} =
    Data.object
      ( Prelude.catMaybes
          [("exact" Data..=) Prelude.<$> exact]
      )
