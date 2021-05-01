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
-- Module      : Network.AWS.APIGateway.Types.TlsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.TlsConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newTlsConfig' smart constructor.
data TlsConfig = TlsConfig'
  { -- | Specifies whether or not API Gateway skips verification that the
    -- certificate for an integration endpoint is issued by a
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority>.
    -- This isn’t recommended, but it enables you to use certificates that are
    -- signed by private certificate authorities, or certificates that are
    -- self-signed. If enabled, API Gateway still performs basic certificate
    -- validation, which includes checking the certificate\'s expiration date,
    -- hostname, and presence of a root certificate authority. Supported only
    -- for @HTTP@ and @HTTP_PROXY@ integrations.
    insecureSkipVerification :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TlsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insecureSkipVerification', 'tlsConfig_insecureSkipVerification' - Specifies whether or not API Gateway skips verification that the
-- certificate for an integration endpoint is issued by a
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority>.
-- This isn’t recommended, but it enables you to use certificates that are
-- signed by private certificate authorities, or certificates that are
-- self-signed. If enabled, API Gateway still performs basic certificate
-- validation, which includes checking the certificate\'s expiration date,
-- hostname, and presence of a root certificate authority. Supported only
-- for @HTTP@ and @HTTP_PROXY@ integrations.
newTlsConfig ::
  TlsConfig
newTlsConfig =
  TlsConfig'
    { insecureSkipVerification =
        Prelude.Nothing
    }

-- | Specifies whether or not API Gateway skips verification that the
-- certificate for an integration endpoint is issued by a
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-supported-certificate-authorities-for-http-endpoints.html supported certificate authority>.
-- This isn’t recommended, but it enables you to use certificates that are
-- signed by private certificate authorities, or certificates that are
-- self-signed. If enabled, API Gateway still performs basic certificate
-- validation, which includes checking the certificate\'s expiration date,
-- hostname, and presence of a root certificate authority. Supported only
-- for @HTTP@ and @HTTP_PROXY@ integrations.
tlsConfig_insecureSkipVerification :: Lens.Lens' TlsConfig (Prelude.Maybe Prelude.Bool)
tlsConfig_insecureSkipVerification = Lens.lens (\TlsConfig' {insecureSkipVerification} -> insecureSkipVerification) (\s@TlsConfig' {} a -> s {insecureSkipVerification = a} :: TlsConfig)

instance Prelude.FromJSON TlsConfig where
  parseJSON =
    Prelude.withObject
      "TlsConfig"
      ( \x ->
          TlsConfig'
            Prelude.<$> (x Prelude..:? "insecureSkipVerification")
      )

instance Prelude.Hashable TlsConfig

instance Prelude.NFData TlsConfig

instance Prelude.ToJSON TlsConfig where
  toJSON TlsConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("insecureSkipVerification" Prelude..=)
              Prelude.<$> insecureSkipVerification
          ]
      )
