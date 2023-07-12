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
-- Module      : Amazonka.OpsWorks.Types.SslConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.SslConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an app\'s SSL configuration.
--
-- /See:/ 'newSslConfiguration' smart constructor.
data SslConfiguration = SslConfiguration'
  { -- | The contents of the certificate\'s domain.crt file.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | Optional. Can be used to specify an intermediate certificate authority
    -- key or client authentication.
    chain :: Prelude.Maybe Prelude.Text,
    -- | The private key; the contents of the certificate\'s domain.kex file.
    privateKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SslConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'sslConfiguration_certificate' - The contents of the certificate\'s domain.crt file.
--
-- 'chain', 'sslConfiguration_chain' - Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
--
-- 'privateKey', 'sslConfiguration_privateKey' - The private key; the contents of the certificate\'s domain.kex file.
newSslConfiguration ::
  SslConfiguration
newSslConfiguration =
  SslConfiguration'
    { certificate = Prelude.Nothing,
      chain = Prelude.Nothing,
      privateKey = Prelude.Nothing
    }

-- | The contents of the certificate\'s domain.crt file.
sslConfiguration_certificate :: Lens.Lens' SslConfiguration (Prelude.Maybe Prelude.Text)
sslConfiguration_certificate = Lens.lens (\SslConfiguration' {certificate} -> certificate) (\s@SslConfiguration' {} a -> s {certificate = a} :: SslConfiguration)

-- | Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
sslConfiguration_chain :: Lens.Lens' SslConfiguration (Prelude.Maybe Prelude.Text)
sslConfiguration_chain = Lens.lens (\SslConfiguration' {chain} -> chain) (\s@SslConfiguration' {} a -> s {chain = a} :: SslConfiguration)

-- | The private key; the contents of the certificate\'s domain.kex file.
sslConfiguration_privateKey :: Lens.Lens' SslConfiguration (Prelude.Maybe Prelude.Text)
sslConfiguration_privateKey = Lens.lens (\SslConfiguration' {privateKey} -> privateKey) (\s@SslConfiguration' {} a -> s {privateKey = a} :: SslConfiguration)

instance Data.FromJSON SslConfiguration where
  parseJSON =
    Data.withObject
      "SslConfiguration"
      ( \x ->
          SslConfiguration'
            Prelude.<$> (x Data..:? "Certificate")
            Prelude.<*> (x Data..:? "Chain")
            Prelude.<*> (x Data..:? "PrivateKey")
      )

instance Prelude.Hashable SslConfiguration where
  hashWithSalt _salt SslConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` chain
      `Prelude.hashWithSalt` privateKey

instance Prelude.NFData SslConfiguration where
  rnf SslConfiguration' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf chain
      `Prelude.seq` Prelude.rnf privateKey

instance Data.ToJSON SslConfiguration where
  toJSON SslConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Certificate" Data..=) Prelude.<$> certificate,
            ("Chain" Data..=) Prelude.<$> chain,
            ("PrivateKey" Data..=) Prelude.<$> privateKey
          ]
      )
