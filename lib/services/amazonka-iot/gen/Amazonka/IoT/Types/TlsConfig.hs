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
-- Module      : Amazonka.IoT.Types.TlsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TlsConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies the TLS configuration for a domain.
--
-- /See:/ 'newTlsConfig' smart constructor.
data TlsConfig = TlsConfig'
  { -- | The security policy for a domain configuration. For more information,
    -- see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/transport-security.html#tls-policy-table Security policies>
    -- in the /Amazon Web Services IoT Core developer guide/.
    securityPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityPolicy', 'tlsConfig_securityPolicy' - The security policy for a domain configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/transport-security.html#tls-policy-table Security policies>
-- in the /Amazon Web Services IoT Core developer guide/.
newTlsConfig ::
  TlsConfig
newTlsConfig =
  TlsConfig' {securityPolicy = Prelude.Nothing}

-- | The security policy for a domain configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/transport-security.html#tls-policy-table Security policies>
-- in the /Amazon Web Services IoT Core developer guide/.
tlsConfig_securityPolicy :: Lens.Lens' TlsConfig (Prelude.Maybe Prelude.Text)
tlsConfig_securityPolicy = Lens.lens (\TlsConfig' {securityPolicy} -> securityPolicy) (\s@TlsConfig' {} a -> s {securityPolicy = a} :: TlsConfig)

instance Data.FromJSON TlsConfig where
  parseJSON =
    Data.withObject
      "TlsConfig"
      ( \x ->
          TlsConfig' Prelude.<$> (x Data..:? "securityPolicy")
      )

instance Prelude.Hashable TlsConfig where
  hashWithSalt _salt TlsConfig' {..} =
    _salt `Prelude.hashWithSalt` securityPolicy

instance Prelude.NFData TlsConfig where
  rnf TlsConfig' {..} = Prelude.rnf securityPolicy

instance Data.ToJSON TlsConfig where
  toJSON TlsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("securityPolicy" Data..=)
              Prelude.<$> securityPolicy
          ]
      )
