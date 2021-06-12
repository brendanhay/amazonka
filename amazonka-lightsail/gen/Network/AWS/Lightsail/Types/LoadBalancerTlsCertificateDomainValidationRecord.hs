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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus

-- | Describes the validation record of each domain name in the SSL\/TLS
-- certificate.
--
-- /See:/ 'newLoadBalancerTlsCertificateDomainValidationRecord' smart constructor.
data LoadBalancerTlsCertificateDomainValidationRecord = LoadBalancerTlsCertificateDomainValidationRecord'
  { -- | A fully qualified domain name in the certificate. For example,
    -- @example.com@.
    name :: Core.Maybe Core.Text,
    -- | The domain name against which your SSL\/TLS certificate was validated.
    domainName :: Core.Maybe Core.Text,
    -- | The validation status. Valid values are listed below.
    validationStatus :: Core.Maybe LoadBalancerTlsCertificateDomainStatus,
    -- | The value for that type.
    value :: Core.Maybe Core.Text,
    -- | The type of validation record. For example, @CNAME@ for domain
    -- validation.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificateDomainValidationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'loadBalancerTlsCertificateDomainValidationRecord_name' - A fully qualified domain name in the certificate. For example,
-- @example.com@.
--
-- 'domainName', 'loadBalancerTlsCertificateDomainValidationRecord_domainName' - The domain name against which your SSL\/TLS certificate was validated.
--
-- 'validationStatus', 'loadBalancerTlsCertificateDomainValidationRecord_validationStatus' - The validation status. Valid values are listed below.
--
-- 'value', 'loadBalancerTlsCertificateDomainValidationRecord_value' - The value for that type.
--
-- 'type'', 'loadBalancerTlsCertificateDomainValidationRecord_type' - The type of validation record. For example, @CNAME@ for domain
-- validation.
newLoadBalancerTlsCertificateDomainValidationRecord ::
  LoadBalancerTlsCertificateDomainValidationRecord
newLoadBalancerTlsCertificateDomainValidationRecord =
  LoadBalancerTlsCertificateDomainValidationRecord'
    { name =
        Core.Nothing,
      domainName = Core.Nothing,
      validationStatus =
        Core.Nothing,
      value = Core.Nothing,
      type' = Core.Nothing
    }

-- | A fully qualified domain name in the certificate. For example,
-- @example.com@.
loadBalancerTlsCertificateDomainValidationRecord_name :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Core.Text)
loadBalancerTlsCertificateDomainValidationRecord_name = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {name} -> name) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {name = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The domain name against which your SSL\/TLS certificate was validated.
loadBalancerTlsCertificateDomainValidationRecord_domainName :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Core.Text)
loadBalancerTlsCertificateDomainValidationRecord_domainName = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {domainName} -> domainName) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {domainName = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The validation status. Valid values are listed below.
loadBalancerTlsCertificateDomainValidationRecord_validationStatus :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe LoadBalancerTlsCertificateDomainStatus)
loadBalancerTlsCertificateDomainValidationRecord_validationStatus = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {validationStatus} -> validationStatus) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {validationStatus = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The value for that type.
loadBalancerTlsCertificateDomainValidationRecord_value :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Core.Text)
loadBalancerTlsCertificateDomainValidationRecord_value = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {value} -> value) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {value = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The type of validation record. For example, @CNAME@ for domain
-- validation.
loadBalancerTlsCertificateDomainValidationRecord_type :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Core.Text)
loadBalancerTlsCertificateDomainValidationRecord_type = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {type'} -> type') (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {type' = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

instance
  Core.FromJSON
    LoadBalancerTlsCertificateDomainValidationRecord
  where
  parseJSON =
    Core.withObject
      "LoadBalancerTlsCertificateDomainValidationRecord"
      ( \x ->
          LoadBalancerTlsCertificateDomainValidationRecord'
            Core.<$> (x Core..:? "name")
              Core.<*> (x Core..:? "domainName")
              Core.<*> (x Core..:? "validationStatus")
              Core.<*> (x Core..:? "value")
              Core.<*> (x Core..:? "type")
      )

instance
  Core.Hashable
    LoadBalancerTlsCertificateDomainValidationRecord

instance
  Core.NFData
    LoadBalancerTlsCertificateDomainValidationRecord
