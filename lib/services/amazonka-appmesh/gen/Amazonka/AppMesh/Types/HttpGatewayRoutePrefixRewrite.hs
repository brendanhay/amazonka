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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRoutePrefixRewrite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRoutePrefixRewrite where

import Amazonka.AppMesh.Types.DefaultGatewayRouteRewrite
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the beginning characters of the route to rewrite.
--
-- /See:/ 'newHttpGatewayRoutePrefixRewrite' smart constructor.
data HttpGatewayRoutePrefixRewrite = HttpGatewayRoutePrefixRewrite'
  { -- | The default prefix used to replace the incoming route prefix when
    -- rewritten.
    defaultPrefix :: Prelude.Maybe DefaultGatewayRouteRewrite,
    -- | The value used to replace the incoming route prefix when rewritten.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRoutePrefixRewrite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultPrefix', 'httpGatewayRoutePrefixRewrite_defaultPrefix' - The default prefix used to replace the incoming route prefix when
-- rewritten.
--
-- 'value', 'httpGatewayRoutePrefixRewrite_value' - The value used to replace the incoming route prefix when rewritten.
newHttpGatewayRoutePrefixRewrite ::
  HttpGatewayRoutePrefixRewrite
newHttpGatewayRoutePrefixRewrite =
  HttpGatewayRoutePrefixRewrite'
    { defaultPrefix =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The default prefix used to replace the incoming route prefix when
-- rewritten.
httpGatewayRoutePrefixRewrite_defaultPrefix :: Lens.Lens' HttpGatewayRoutePrefixRewrite (Prelude.Maybe DefaultGatewayRouteRewrite)
httpGatewayRoutePrefixRewrite_defaultPrefix = Lens.lens (\HttpGatewayRoutePrefixRewrite' {defaultPrefix} -> defaultPrefix) (\s@HttpGatewayRoutePrefixRewrite' {} a -> s {defaultPrefix = a} :: HttpGatewayRoutePrefixRewrite)

-- | The value used to replace the incoming route prefix when rewritten.
httpGatewayRoutePrefixRewrite_value :: Lens.Lens' HttpGatewayRoutePrefixRewrite (Prelude.Maybe Prelude.Text)
httpGatewayRoutePrefixRewrite_value = Lens.lens (\HttpGatewayRoutePrefixRewrite' {value} -> value) (\s@HttpGatewayRoutePrefixRewrite' {} a -> s {value = a} :: HttpGatewayRoutePrefixRewrite)

instance Data.FromJSON HttpGatewayRoutePrefixRewrite where
  parseJSON =
    Data.withObject
      "HttpGatewayRoutePrefixRewrite"
      ( \x ->
          HttpGatewayRoutePrefixRewrite'
            Prelude.<$> (x Data..:? "defaultPrefix")
            Prelude.<*> (x Data..:? "value")
      )

instance
  Prelude.Hashable
    HttpGatewayRoutePrefixRewrite
  where
  hashWithSalt _salt HttpGatewayRoutePrefixRewrite' {..} =
    _salt `Prelude.hashWithSalt` defaultPrefix
      `Prelude.hashWithSalt` value

instance Prelude.NFData HttpGatewayRoutePrefixRewrite where
  rnf HttpGatewayRoutePrefixRewrite' {..} =
    Prelude.rnf defaultPrefix
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON HttpGatewayRoutePrefixRewrite where
  toJSON HttpGatewayRoutePrefixRewrite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultPrefix" Data..=) Prelude.<$> defaultPrefix,
            ("value" Data..=) Prelude.<$> value
          ]
      )
