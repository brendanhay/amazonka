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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus

-- | Contains information about the domain names on an SSL\/TLS certificate
-- that you will use to validate domain ownership.
--
-- /See:/ 'newLoadBalancerTlsCertificateDomainValidationOption' smart constructor.
data LoadBalancerTlsCertificateDomainValidationOption = LoadBalancerTlsCertificateDomainValidationOption'
  { -- | The fully qualified domain name in the certificate request.
    domainName :: Core.Maybe Core.Text,
    -- | The status of the domain validation. Valid values are listed below.
    validationStatus :: Core.Maybe LoadBalancerTlsCertificateDomainStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificateDomainValidationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'loadBalancerTlsCertificateDomainValidationOption_domainName' - The fully qualified domain name in the certificate request.
--
-- 'validationStatus', 'loadBalancerTlsCertificateDomainValidationOption_validationStatus' - The status of the domain validation. Valid values are listed below.
newLoadBalancerTlsCertificateDomainValidationOption ::
  LoadBalancerTlsCertificateDomainValidationOption
newLoadBalancerTlsCertificateDomainValidationOption =
  LoadBalancerTlsCertificateDomainValidationOption'
    { domainName =
        Core.Nothing,
      validationStatus =
        Core.Nothing
    }

-- | The fully qualified domain name in the certificate request.
loadBalancerTlsCertificateDomainValidationOption_domainName :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationOption (Core.Maybe Core.Text)
loadBalancerTlsCertificateDomainValidationOption_domainName = Lens.lens (\LoadBalancerTlsCertificateDomainValidationOption' {domainName} -> domainName) (\s@LoadBalancerTlsCertificateDomainValidationOption' {} a -> s {domainName = a} :: LoadBalancerTlsCertificateDomainValidationOption)

-- | The status of the domain validation. Valid values are listed below.
loadBalancerTlsCertificateDomainValidationOption_validationStatus :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationOption (Core.Maybe LoadBalancerTlsCertificateDomainStatus)
loadBalancerTlsCertificateDomainValidationOption_validationStatus = Lens.lens (\LoadBalancerTlsCertificateDomainValidationOption' {validationStatus} -> validationStatus) (\s@LoadBalancerTlsCertificateDomainValidationOption' {} a -> s {validationStatus = a} :: LoadBalancerTlsCertificateDomainValidationOption)

instance
  Core.FromJSON
    LoadBalancerTlsCertificateDomainValidationOption
  where
  parseJSON =
    Core.withObject
      "LoadBalancerTlsCertificateDomainValidationOption"
      ( \x ->
          LoadBalancerTlsCertificateDomainValidationOption'
            Core.<$> (x Core..:? "domainName")
              Core.<*> (x Core..:? "validationStatus")
      )

instance
  Core.Hashable
    LoadBalancerTlsCertificateDomainValidationOption

instance
  Core.NFData
    LoadBalancerTlsCertificateDomainValidationOption
