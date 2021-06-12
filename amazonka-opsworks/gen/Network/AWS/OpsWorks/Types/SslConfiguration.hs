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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an app\'s SSL configuration.
--
-- /See:/ 'newSslConfiguration' smart constructor.
data SslConfiguration = SslConfiguration'
  { -- | The private key; the contents of the certificate\'s domain.kex file.
    privateKey :: Core.Maybe Core.Text,
    -- | The contents of the certificate\'s domain.crt file.
    certificate :: Core.Maybe Core.Text,
    -- | Optional. Can be used to specify an intermediate certificate authority
    -- key or client authentication.
    chain :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { privateKey = Core.Nothing,
      certificate = Core.Nothing,
      chain = Core.Nothing
    }

-- | The private key; the contents of the certificate\'s domain.kex file.
sslConfiguration_privateKey :: Lens.Lens' SslConfiguration (Core.Maybe Core.Text)
sslConfiguration_privateKey = Lens.lens (\SslConfiguration' {privateKey} -> privateKey) (\s@SslConfiguration' {} a -> s {privateKey = a} :: SslConfiguration)

-- | The contents of the certificate\'s domain.crt file.
sslConfiguration_certificate :: Lens.Lens' SslConfiguration (Core.Maybe Core.Text)
sslConfiguration_certificate = Lens.lens (\SslConfiguration' {certificate} -> certificate) (\s@SslConfiguration' {} a -> s {certificate = a} :: SslConfiguration)

-- | Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
sslConfiguration_chain :: Lens.Lens' SslConfiguration (Core.Maybe Core.Text)
sslConfiguration_chain = Lens.lens (\SslConfiguration' {chain} -> chain) (\s@SslConfiguration' {} a -> s {chain = a} :: SslConfiguration)

instance Core.FromJSON SslConfiguration where
  parseJSON =
    Core.withObject
      "SslConfiguration"
      ( \x ->
          SslConfiguration'
            Core.<$> (x Core..:? "PrivateKey")
            Core.<*> (x Core..:? "Certificate")
            Core.<*> (x Core..:? "Chain")
      )

instance Core.Hashable SslConfiguration

instance Core.NFData SslConfiguration

instance Core.ToJSON SslConfiguration where
  toJSON SslConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PrivateKey" Core..=) Core.<$> privateKey,
            ("Certificate" Core..=) Core.<$> certificate,
            ("Chain" Core..=) Core.<$> chain
          ]
      )
