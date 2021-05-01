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
-- Module      : Network.AWS.OpsWorks.Types.SslConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SslConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an app\'s SSL configuration.
--
-- /See:/ 'newSslConfiguration' smart constructor.
data SslConfiguration = SslConfiguration'
  { -- | The private key; the contents of the certificate\'s domain.kex file.
    privateKey :: Prelude.Maybe Prelude.Text,
    -- | The contents of the certificate\'s domain.crt file.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | Optional. Can be used to specify an intermediate certificate authority
    -- key or client authentication.
    chain :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SslConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateKey', 'sslConfiguration_privateKey' - The private key; the contents of the certificate\'s domain.kex file.
--
-- 'certificate', 'sslConfiguration_certificate' - The contents of the certificate\'s domain.crt file.
--
-- 'chain', 'sslConfiguration_chain' - Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
newSslConfiguration ::
  SslConfiguration
newSslConfiguration =
  SslConfiguration'
    { privateKey = Prelude.Nothing,
      certificate = Prelude.Nothing,
      chain = Prelude.Nothing
    }

-- | The private key; the contents of the certificate\'s domain.kex file.
sslConfiguration_privateKey :: Lens.Lens' SslConfiguration (Prelude.Maybe Prelude.Text)
sslConfiguration_privateKey = Lens.lens (\SslConfiguration' {privateKey} -> privateKey) (\s@SslConfiguration' {} a -> s {privateKey = a} :: SslConfiguration)

-- | The contents of the certificate\'s domain.crt file.
sslConfiguration_certificate :: Lens.Lens' SslConfiguration (Prelude.Maybe Prelude.Text)
sslConfiguration_certificate = Lens.lens (\SslConfiguration' {certificate} -> certificate) (\s@SslConfiguration' {} a -> s {certificate = a} :: SslConfiguration)

-- | Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
sslConfiguration_chain :: Lens.Lens' SslConfiguration (Prelude.Maybe Prelude.Text)
sslConfiguration_chain = Lens.lens (\SslConfiguration' {chain} -> chain) (\s@SslConfiguration' {} a -> s {chain = a} :: SslConfiguration)

instance Prelude.FromJSON SslConfiguration where
  parseJSON =
    Prelude.withObject
      "SslConfiguration"
      ( \x ->
          SslConfiguration'
            Prelude.<$> (x Prelude..:? "PrivateKey")
            Prelude.<*> (x Prelude..:? "Certificate")
            Prelude.<*> (x Prelude..:? "Chain")
      )

instance Prelude.Hashable SslConfiguration

instance Prelude.NFData SslConfiguration

instance Prelude.ToJSON SslConfiguration where
  toJSON SslConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PrivateKey" Prelude..=) Prelude.<$> privateKey,
            ("Certificate" Prelude..=) Prelude.<$> certificate,
            ("Chain" Prelude..=) Prelude.<$> chain
          ]
      )
