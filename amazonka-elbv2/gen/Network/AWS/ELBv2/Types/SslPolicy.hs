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
-- Module      : Network.AWS.ELBv2.Types.SslPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SslPolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.Cipher
import qualified Network.AWS.Lens as Lens

-- | Information about a policy used for SSL negotiation.
--
-- /See:/ 'newSslPolicy' smart constructor.
data SslPolicy = SslPolicy'
  { -- | The ciphers.
    ciphers :: Core.Maybe [Cipher],
    -- | The name of the policy.
    name :: Core.Maybe Core.Text,
    -- | The protocols.
    sslProtocols :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SslPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ciphers', 'sslPolicy_ciphers' - The ciphers.
--
-- 'name', 'sslPolicy_name' - The name of the policy.
--
-- 'sslProtocols', 'sslPolicy_sslProtocols' - The protocols.
newSslPolicy ::
  SslPolicy
newSslPolicy =
  SslPolicy'
    { ciphers = Core.Nothing,
      name = Core.Nothing,
      sslProtocols = Core.Nothing
    }

-- | The ciphers.
sslPolicy_ciphers :: Lens.Lens' SslPolicy (Core.Maybe [Cipher])
sslPolicy_ciphers = Lens.lens (\SslPolicy' {ciphers} -> ciphers) (\s@SslPolicy' {} a -> s {ciphers = a} :: SslPolicy) Core.. Lens.mapping Lens._Coerce

-- | The name of the policy.
sslPolicy_name :: Lens.Lens' SslPolicy (Core.Maybe Core.Text)
sslPolicy_name = Lens.lens (\SslPolicy' {name} -> name) (\s@SslPolicy' {} a -> s {name = a} :: SslPolicy)

-- | The protocols.
sslPolicy_sslProtocols :: Lens.Lens' SslPolicy (Core.Maybe [Core.Text])
sslPolicy_sslProtocols = Lens.lens (\SslPolicy' {sslProtocols} -> sslProtocols) (\s@SslPolicy' {} a -> s {sslProtocols = a} :: SslPolicy) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML SslPolicy where
  parseXML x =
    SslPolicy'
      Core.<$> ( x Core..@? "Ciphers" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Name")
      Core.<*> ( x Core..@? "SslProtocols" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable SslPolicy

instance Core.NFData SslPolicy
