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
-- Module      : Amazonka.ELBV2.Types.SslPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.SslPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types.Cipher
import qualified Amazonka.Prelude as Prelude

-- | Information about a policy used for SSL negotiation.
--
-- /See:/ 'newSslPolicy' smart constructor.
data SslPolicy = SslPolicy'
  { -- | The name of the policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The supported load balancers.
    supportedLoadBalancerTypes :: Prelude.Maybe [Prelude.Text],
    -- | The ciphers.
    ciphers :: Prelude.Maybe [Cipher],
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
-- 'name', 'sslPolicy_name' - The name of the policy.
--
-- 'supportedLoadBalancerTypes', 'sslPolicy_supportedLoadBalancerTypes' - The supported load balancers.
--
-- 'ciphers', 'sslPolicy_ciphers' - The ciphers.
--
-- 'sslProtocols', 'sslPolicy_sslProtocols' - The protocols.
newSslPolicy ::
  SslPolicy
newSslPolicy =
  SslPolicy'
    { name = Prelude.Nothing,
      supportedLoadBalancerTypes = Prelude.Nothing,
      ciphers = Prelude.Nothing,
      sslProtocols = Prelude.Nothing
    }

-- | The name of the policy.
sslPolicy_name :: Lens.Lens' SslPolicy (Prelude.Maybe Prelude.Text)
sslPolicy_name = Lens.lens (\SslPolicy' {name} -> name) (\s@SslPolicy' {} a -> s {name = a} :: SslPolicy)

-- | The supported load balancers.
sslPolicy_supportedLoadBalancerTypes :: Lens.Lens' SslPolicy (Prelude.Maybe [Prelude.Text])
sslPolicy_supportedLoadBalancerTypes = Lens.lens (\SslPolicy' {supportedLoadBalancerTypes} -> supportedLoadBalancerTypes) (\s@SslPolicy' {} a -> s {supportedLoadBalancerTypes = a} :: SslPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The ciphers.
sslPolicy_ciphers :: Lens.Lens' SslPolicy (Prelude.Maybe [Cipher])
sslPolicy_ciphers = Lens.lens (\SslPolicy' {ciphers} -> ciphers) (\s@SslPolicy' {} a -> s {ciphers = a} :: SslPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The protocols.
sslPolicy_sslProtocols :: Lens.Lens' SslPolicy (Prelude.Maybe [Prelude.Text])
sslPolicy_sslProtocols = Lens.lens (\SslPolicy' {sslProtocols} -> sslProtocols) (\s@SslPolicy' {} a -> s {sslProtocols = a} :: SslPolicy) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML SslPolicy where
  parseXML x =
    SslPolicy'
      Prelude.<$> (x Core..@? "Name")
      Prelude.<*> ( x Core..@? "SupportedLoadBalancerTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "Ciphers" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "SslProtocols" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable SslPolicy where
  hashWithSalt _salt SslPolicy' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` supportedLoadBalancerTypes
      `Prelude.hashWithSalt` ciphers
      `Prelude.hashWithSalt` sslProtocols

instance Prelude.NFData SslPolicy where
  rnf SslPolicy' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedLoadBalancerTypes
      `Prelude.seq` Prelude.rnf ciphers
      `Prelude.seq` Prelude.rnf sslProtocols
