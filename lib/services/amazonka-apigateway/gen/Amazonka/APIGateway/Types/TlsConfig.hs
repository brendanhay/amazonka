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
-- Module      : Amazonka.APIGateway.Types.TlsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.TlsConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the TLS configuration for an integration.
--
-- /See:/ 'newTlsConfig' smart constructor.
data TlsConfig = TlsConfig'
  { -- | Specifies whether or not API Gateway skips verification that the
    -- certificate for an integration endpoint is issued by a supported
    -- certificate authority. This isn’t recommended, but it enables you to use
    -- certificates that are signed by private certificate authorities, or
    -- certificates that are self-signed. If enabled, API Gateway still
    -- performs basic certificate validation, which includes checking the
    -- certificate\'s expiration date, hostname, and presence of a root
    -- certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@
    -- integrations.
    --
    -- Enabling @insecureSkipVerification@ isn\'t recommended, especially for
    -- integrations with public HTTPS endpoints. If you enable
    -- @insecureSkipVerification@, you increase the risk of man-in-the-middle
    -- attacks.
    insecureSkipVerification :: Prelude.Maybe Prelude.Bool
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
-- 'insecureSkipVerification', 'tlsConfig_insecureSkipVerification' - Specifies whether or not API Gateway skips verification that the
-- certificate for an integration endpoint is issued by a supported
-- certificate authority. This isn’t recommended, but it enables you to use
-- certificates that are signed by private certificate authorities, or
-- certificates that are self-signed. If enabled, API Gateway still
-- performs basic certificate validation, which includes checking the
-- certificate\'s expiration date, hostname, and presence of a root
-- certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@
-- integrations.
--
-- Enabling @insecureSkipVerification@ isn\'t recommended, especially for
-- integrations with public HTTPS endpoints. If you enable
-- @insecureSkipVerification@, you increase the risk of man-in-the-middle
-- attacks.
newTlsConfig ::
  TlsConfig
newTlsConfig =
  TlsConfig'
    { insecureSkipVerification =
        Prelude.Nothing
    }

-- | Specifies whether or not API Gateway skips verification that the
-- certificate for an integration endpoint is issued by a supported
-- certificate authority. This isn’t recommended, but it enables you to use
-- certificates that are signed by private certificate authorities, or
-- certificates that are self-signed. If enabled, API Gateway still
-- performs basic certificate validation, which includes checking the
-- certificate\'s expiration date, hostname, and presence of a root
-- certificate authority. Supported only for @HTTP@ and @HTTP_PROXY@
-- integrations.
--
-- Enabling @insecureSkipVerification@ isn\'t recommended, especially for
-- integrations with public HTTPS endpoints. If you enable
-- @insecureSkipVerification@, you increase the risk of man-in-the-middle
-- attacks.
tlsConfig_insecureSkipVerification :: Lens.Lens' TlsConfig (Prelude.Maybe Prelude.Bool)
tlsConfig_insecureSkipVerification = Lens.lens (\TlsConfig' {insecureSkipVerification} -> insecureSkipVerification) (\s@TlsConfig' {} a -> s {insecureSkipVerification = a} :: TlsConfig)

instance Data.FromJSON TlsConfig where
  parseJSON =
    Data.withObject
      "TlsConfig"
      ( \x ->
          TlsConfig'
            Prelude.<$> (x Data..:? "insecureSkipVerification")
      )

instance Prelude.Hashable TlsConfig where
  hashWithSalt _salt TlsConfig' {..} =
    _salt
      `Prelude.hashWithSalt` insecureSkipVerification

instance Prelude.NFData TlsConfig where
  rnf TlsConfig' {..} =
    Prelude.rnf insecureSkipVerification

instance Data.ToJSON TlsConfig where
  toJSON TlsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("insecureSkipVerification" Data..=)
              Prelude.<$> insecureSkipVerification
          ]
      )
