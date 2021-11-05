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
-- Module      : Network.AWS.AppFlow.Types.SAPODataConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.SAPODataConnectorProfileCredentials where

import Network.AWS.AppFlow.Types.BasicAuthCredentials
import Network.AWS.AppFlow.Types.OAuthCredentials
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The connector-specific profile credentials required when using SAPOData.
--
-- /See:/ 'newSAPODataConnectorProfileCredentials' smart constructor.
data SAPODataConnectorProfileCredentials = SAPODataConnectorProfileCredentials'
  { -- | The SAPOData OAuth type authentication credentials.
    oAuthCredentials :: Prelude.Maybe OAuthCredentials,
    -- | The SAPOData basic authentication credentials.
    basicAuthCredentials :: Prelude.Maybe BasicAuthCredentials
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
-- 'oAuthCredentials', 'sAPODataConnectorProfileCredentials_oAuthCredentials' - The SAPOData OAuth type authentication credentials.
--
-- 'basicAuthCredentials', 'sAPODataConnectorProfileCredentials_basicAuthCredentials' - The SAPOData basic authentication credentials.
newSAPODataConnectorProfileCredentials ::
  SAPODataConnectorProfileCredentials
newSAPODataConnectorProfileCredentials =
  SAPODataConnectorProfileCredentials'
    { oAuthCredentials =
        Prelude.Nothing,
      basicAuthCredentials = Prelude.Nothing
    }

-- | The SAPOData OAuth type authentication credentials.
sAPODataConnectorProfileCredentials_oAuthCredentials :: Lens.Lens' SAPODataConnectorProfileCredentials (Prelude.Maybe OAuthCredentials)
sAPODataConnectorProfileCredentials_oAuthCredentials = Lens.lens (\SAPODataConnectorProfileCredentials' {oAuthCredentials} -> oAuthCredentials) (\s@SAPODataConnectorProfileCredentials' {} a -> s {oAuthCredentials = a} :: SAPODataConnectorProfileCredentials)

-- | The SAPOData basic authentication credentials.
sAPODataConnectorProfileCredentials_basicAuthCredentials :: Lens.Lens' SAPODataConnectorProfileCredentials (Prelude.Maybe BasicAuthCredentials)
sAPODataConnectorProfileCredentials_basicAuthCredentials = Lens.lens (\SAPODataConnectorProfileCredentials' {basicAuthCredentials} -> basicAuthCredentials) (\s@SAPODataConnectorProfileCredentials' {} a -> s {basicAuthCredentials = a} :: SAPODataConnectorProfileCredentials)

instance
  Prelude.Hashable
    SAPODataConnectorProfileCredentials

instance
  Prelude.NFData
    SAPODataConnectorProfileCredentials

instance
  Core.ToJSON
    SAPODataConnectorProfileCredentials
  where
  toJSON SAPODataConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("oAuthCredentials" Core..=)
              Prelude.<$> oAuthCredentials,
            ("basicAuthCredentials" Core..=)
              Prelude.<$> basicAuthCredentials
          ]
      )
