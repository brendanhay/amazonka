{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionApiKeyAuthResponseParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionApiKeyAuthResponseParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the authorization parameters for the connection if API Key is
-- specified as the authorization type.
--
-- /See:/ 'newConnectionApiKeyAuthResponseParameters' smart constructor.
data ConnectionApiKeyAuthResponseParameters = ConnectionApiKeyAuthResponseParameters'
  { -- | The name of the header to use for the @APIKeyValue@ used for
    -- authorization.
    apiKeyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConnectionApiKeyAuthResponseParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyName', 'connectionApiKeyAuthResponseParameters_apiKeyName' - The name of the header to use for the @APIKeyValue@ used for
-- authorization.
newConnectionApiKeyAuthResponseParameters ::
  ConnectionApiKeyAuthResponseParameters
newConnectionApiKeyAuthResponseParameters =
  ConnectionApiKeyAuthResponseParameters'
    { apiKeyName =
        Prelude.Nothing
    }

-- | The name of the header to use for the @APIKeyValue@ used for
-- authorization.
connectionApiKeyAuthResponseParameters_apiKeyName :: Lens.Lens' ConnectionApiKeyAuthResponseParameters (Prelude.Maybe Prelude.Text)
connectionApiKeyAuthResponseParameters_apiKeyName = Lens.lens (\ConnectionApiKeyAuthResponseParameters' {apiKeyName} -> apiKeyName) (\s@ConnectionApiKeyAuthResponseParameters' {} a -> s {apiKeyName = a} :: ConnectionApiKeyAuthResponseParameters)

instance
  Prelude.FromJSON
    ConnectionApiKeyAuthResponseParameters
  where
  parseJSON =
    Prelude.withObject
      "ConnectionApiKeyAuthResponseParameters"
      ( \x ->
          ConnectionApiKeyAuthResponseParameters'
            Prelude.<$> (x Prelude..:? "ApiKeyName")
      )

instance
  Prelude.Hashable
    ConnectionApiKeyAuthResponseParameters

instance
  Prelude.NFData
    ConnectionApiKeyAuthResponseParameters
