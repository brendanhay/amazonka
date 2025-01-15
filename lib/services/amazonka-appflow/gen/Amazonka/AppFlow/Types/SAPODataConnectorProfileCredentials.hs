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
-- Module      : Amazonka.AppFlow.Types.SAPODataConnectorProfileCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SAPODataConnectorProfileCredentials where

import Amazonka.AppFlow.Types.BasicAuthCredentials
import Amazonka.AppFlow.Types.OAuthCredentials
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using SAPOData.
--
-- /See:/ 'newSAPODataConnectorProfileCredentials' smart constructor.
data SAPODataConnectorProfileCredentials = SAPODataConnectorProfileCredentials'
  { -- | The SAPOData basic authentication credentials.
    basicAuthCredentials :: Prelude.Maybe BasicAuthCredentials,
    -- | The SAPOData OAuth type authentication credentials.
    oAuthCredentials :: Prelude.Maybe OAuthCredentials
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAPODataConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basicAuthCredentials', 'sAPODataConnectorProfileCredentials_basicAuthCredentials' - The SAPOData basic authentication credentials.
--
-- 'oAuthCredentials', 'sAPODataConnectorProfileCredentials_oAuthCredentials' - The SAPOData OAuth type authentication credentials.
newSAPODataConnectorProfileCredentials ::
  SAPODataConnectorProfileCredentials
newSAPODataConnectorProfileCredentials =
  SAPODataConnectorProfileCredentials'
    { basicAuthCredentials =
        Prelude.Nothing,
      oAuthCredentials = Prelude.Nothing
    }

-- | The SAPOData basic authentication credentials.
sAPODataConnectorProfileCredentials_basicAuthCredentials :: Lens.Lens' SAPODataConnectorProfileCredentials (Prelude.Maybe BasicAuthCredentials)
sAPODataConnectorProfileCredentials_basicAuthCredentials = Lens.lens (\SAPODataConnectorProfileCredentials' {basicAuthCredentials} -> basicAuthCredentials) (\s@SAPODataConnectorProfileCredentials' {} a -> s {basicAuthCredentials = a} :: SAPODataConnectorProfileCredentials)

-- | The SAPOData OAuth type authentication credentials.
sAPODataConnectorProfileCredentials_oAuthCredentials :: Lens.Lens' SAPODataConnectorProfileCredentials (Prelude.Maybe OAuthCredentials)
sAPODataConnectorProfileCredentials_oAuthCredentials = Lens.lens (\SAPODataConnectorProfileCredentials' {oAuthCredentials} -> oAuthCredentials) (\s@SAPODataConnectorProfileCredentials' {} a -> s {oAuthCredentials = a} :: SAPODataConnectorProfileCredentials)

instance
  Prelude.Hashable
    SAPODataConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    SAPODataConnectorProfileCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` basicAuthCredentials
        `Prelude.hashWithSalt` oAuthCredentials

instance
  Prelude.NFData
    SAPODataConnectorProfileCredentials
  where
  rnf SAPODataConnectorProfileCredentials' {..} =
    Prelude.rnf basicAuthCredentials `Prelude.seq`
      Prelude.rnf oAuthCredentials

instance
  Data.ToJSON
    SAPODataConnectorProfileCredentials
  where
  toJSON SAPODataConnectorProfileCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("basicAuthCredentials" Data..=)
              Prelude.<$> basicAuthCredentials,
            ("oAuthCredentials" Data..=)
              Prelude.<$> oAuthCredentials
          ]
      )
