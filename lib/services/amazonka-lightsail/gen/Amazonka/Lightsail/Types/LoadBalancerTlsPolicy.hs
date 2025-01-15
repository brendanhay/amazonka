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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the TLS security policies that are available for Lightsail
-- load balancers.
--
-- For more information about load balancer TLS security policies, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configure-load-balancer-tls-security-policy Configuring TLS security policies on your Amazon Lightsail load balancers>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newLoadBalancerTlsPolicy' smart constructor.
data LoadBalancerTlsPolicy = LoadBalancerTlsPolicy'
  { -- | The ciphers used by the TLS security policy.
    --
    -- The ciphers are listed in order of preference.
    ciphers :: Prelude.Maybe [Prelude.Text],
    -- | The description of the TLS security policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that indicates whether the TLS security policy is the
    -- default.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The name of the TLS security policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The protocols used in a given TLS security policy.
    protocols :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerTlsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ciphers', 'loadBalancerTlsPolicy_ciphers' - The ciphers used by the TLS security policy.
--
-- The ciphers are listed in order of preference.
--
-- 'description', 'loadBalancerTlsPolicy_description' - The description of the TLS security policy.
--
-- 'isDefault', 'loadBalancerTlsPolicy_isDefault' - A Boolean value that indicates whether the TLS security policy is the
-- default.
--
-- 'name', 'loadBalancerTlsPolicy_name' - The name of the TLS security policy.
--
-- 'protocols', 'loadBalancerTlsPolicy_protocols' - The protocols used in a given TLS security policy.
newLoadBalancerTlsPolicy ::
  LoadBalancerTlsPolicy
newLoadBalancerTlsPolicy =
  LoadBalancerTlsPolicy'
    { ciphers = Prelude.Nothing,
      description = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      name = Prelude.Nothing,
      protocols = Prelude.Nothing
    }

-- | The ciphers used by the TLS security policy.
--
-- The ciphers are listed in order of preference.
loadBalancerTlsPolicy_ciphers :: Lens.Lens' LoadBalancerTlsPolicy (Prelude.Maybe [Prelude.Text])
loadBalancerTlsPolicy_ciphers = Lens.lens (\LoadBalancerTlsPolicy' {ciphers} -> ciphers) (\s@LoadBalancerTlsPolicy' {} a -> s {ciphers = a} :: LoadBalancerTlsPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The description of the TLS security policy.
loadBalancerTlsPolicy_description :: Lens.Lens' LoadBalancerTlsPolicy (Prelude.Maybe Prelude.Text)
loadBalancerTlsPolicy_description = Lens.lens (\LoadBalancerTlsPolicy' {description} -> description) (\s@LoadBalancerTlsPolicy' {} a -> s {description = a} :: LoadBalancerTlsPolicy)

-- | A Boolean value that indicates whether the TLS security policy is the
-- default.
loadBalancerTlsPolicy_isDefault :: Lens.Lens' LoadBalancerTlsPolicy (Prelude.Maybe Prelude.Bool)
loadBalancerTlsPolicy_isDefault = Lens.lens (\LoadBalancerTlsPolicy' {isDefault} -> isDefault) (\s@LoadBalancerTlsPolicy' {} a -> s {isDefault = a} :: LoadBalancerTlsPolicy)

-- | The name of the TLS security policy.
loadBalancerTlsPolicy_name :: Lens.Lens' LoadBalancerTlsPolicy (Prelude.Maybe Prelude.Text)
loadBalancerTlsPolicy_name = Lens.lens (\LoadBalancerTlsPolicy' {name} -> name) (\s@LoadBalancerTlsPolicy' {} a -> s {name = a} :: LoadBalancerTlsPolicy)

-- | The protocols used in a given TLS security policy.
loadBalancerTlsPolicy_protocols :: Lens.Lens' LoadBalancerTlsPolicy (Prelude.Maybe [Prelude.Text])
loadBalancerTlsPolicy_protocols = Lens.lens (\LoadBalancerTlsPolicy' {protocols} -> protocols) (\s@LoadBalancerTlsPolicy' {} a -> s {protocols = a} :: LoadBalancerTlsPolicy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LoadBalancerTlsPolicy where
  parseJSON =
    Data.withObject
      "LoadBalancerTlsPolicy"
      ( \x ->
          LoadBalancerTlsPolicy'
            Prelude.<$> (x Data..:? "ciphers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "isDefault")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "protocols" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LoadBalancerTlsPolicy where
  hashWithSalt _salt LoadBalancerTlsPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` ciphers
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocols

instance Prelude.NFData LoadBalancerTlsPolicy where
  rnf LoadBalancerTlsPolicy' {..} =
    Prelude.rnf ciphers `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf isDefault `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf protocols
