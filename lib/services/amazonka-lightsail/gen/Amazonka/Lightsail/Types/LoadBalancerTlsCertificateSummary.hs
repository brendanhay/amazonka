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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of SSL\/TLS certificate metadata.
--
-- /See:/ 'newLoadBalancerTlsCertificateSummary' smart constructor.
data LoadBalancerTlsCertificateSummary = LoadBalancerTlsCertificateSummary'
  { -- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
    -- balancer.
    isAttached :: Prelude.Maybe Prelude.Bool,
    -- | The name of the SSL\/TLS certificate.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isAttached', 'loadBalancerTlsCertificateSummary_isAttached' - When @true@, the SSL\/TLS certificate is attached to the Lightsail load
-- balancer.
--
-- 'name', 'loadBalancerTlsCertificateSummary_name' - The name of the SSL\/TLS certificate.
newLoadBalancerTlsCertificateSummary ::
  LoadBalancerTlsCertificateSummary
newLoadBalancerTlsCertificateSummary =
  LoadBalancerTlsCertificateSummary'
    { isAttached =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
-- balancer.
loadBalancerTlsCertificateSummary_isAttached :: Lens.Lens' LoadBalancerTlsCertificateSummary (Prelude.Maybe Prelude.Bool)
loadBalancerTlsCertificateSummary_isAttached = Lens.lens (\LoadBalancerTlsCertificateSummary' {isAttached} -> isAttached) (\s@LoadBalancerTlsCertificateSummary' {} a -> s {isAttached = a} :: LoadBalancerTlsCertificateSummary)

-- | The name of the SSL\/TLS certificate.
loadBalancerTlsCertificateSummary_name :: Lens.Lens' LoadBalancerTlsCertificateSummary (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificateSummary_name = Lens.lens (\LoadBalancerTlsCertificateSummary' {name} -> name) (\s@LoadBalancerTlsCertificateSummary' {} a -> s {name = a} :: LoadBalancerTlsCertificateSummary)

instance
  Data.FromJSON
    LoadBalancerTlsCertificateSummary
  where
  parseJSON =
    Data.withObject
      "LoadBalancerTlsCertificateSummary"
      ( \x ->
          LoadBalancerTlsCertificateSummary'
            Prelude.<$> (x Data..:? "isAttached")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    LoadBalancerTlsCertificateSummary
  where
  hashWithSalt
    _salt
    LoadBalancerTlsCertificateSummary' {..} =
      _salt `Prelude.hashWithSalt` isAttached
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    LoadBalancerTlsCertificateSummary
  where
  rnf LoadBalancerTlsCertificateSummary' {..} =
    Prelude.rnf isAttached
      `Prelude.seq` Prelude.rnf name
