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
import qualified Network.AWS.Prelude as Prelude

-- | Information about a policy used for SSL negotiation.
--
-- /See:/ 'newSslPolicy' smart constructor.
data SslPolicy = SslPolicy'
  { -- | The ciphers.
    ciphers :: Prelude.Maybe [Cipher],
    -- | The name of the policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The protocols.
    sslProtocols :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { ciphers = Prelude.Nothing,
      name = Prelude.Nothing,
      sslProtocols = Prelude.Nothing
    }

-- | The ciphers.
sslPolicy_ciphers :: Lens.Lens' SslPolicy (Prelude.Maybe [Cipher])
sslPolicy_ciphers = Lens.lens (\SslPolicy' {ciphers} -> ciphers) (\s@SslPolicy' {} a -> s {ciphers = a} :: SslPolicy) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the policy.
sslPolicy_name :: Lens.Lens' SslPolicy (Prelude.Maybe Prelude.Text)
sslPolicy_name = Lens.lens (\SslPolicy' {name} -> name) (\s@SslPolicy' {} a -> s {name = a} :: SslPolicy)

-- | The protocols.
sslPolicy_sslProtocols :: Lens.Lens' SslPolicy (Prelude.Maybe [Prelude.Text])
sslPolicy_sslProtocols = Lens.lens (\SslPolicy' {sslProtocols} -> sslProtocols) (\s@SslPolicy' {} a -> s {sslProtocols = a} :: SslPolicy) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromXML SslPolicy where
  parseXML x =
    SslPolicy'
      Prelude.<$> ( x Core..@? "Ciphers" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Name")
      Prelude.<*> ( x Core..@? "SslProtocols" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable SslPolicy

instance Prelude.NFData SslPolicy
