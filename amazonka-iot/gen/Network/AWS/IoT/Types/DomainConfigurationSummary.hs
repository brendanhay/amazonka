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
-- Module      : Network.AWS.IoT.Types.DomainConfigurationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DomainConfigurationSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ServiceType
import qualified Network.AWS.Lens as Lens

-- | The summary of a domain configuration. A domain configuration specifies
-- custom IoT-specific information about a domain. A domain configuration
-- can be associated with an AWS-managed domain (for example,
-- dbc123defghijk.iot.us-west-2.amazonaws.com), a customer managed domain,
-- or a default endpoint.
--
-- -   Data
--
-- -   Jobs
--
-- -   CredentialProvider
--
-- The domain configuration feature is in public preview and is subject to
-- change.
--
-- /See:/ 'newDomainConfigurationSummary' smart constructor.
data DomainConfigurationSummary = DomainConfigurationSummary'
  { -- | The ARN of the domain configuration.
    domainConfigurationArn :: Core.Maybe Core.Text,
    -- | The name of the domain configuration. This value must be unique to a
    -- region.
    domainConfigurationName :: Core.Maybe Core.Text,
    -- | The type of service delivered by the endpoint.
    serviceType :: Core.Maybe ServiceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurationArn', 'domainConfigurationSummary_domainConfigurationArn' - The ARN of the domain configuration.
--
-- 'domainConfigurationName', 'domainConfigurationSummary_domainConfigurationName' - The name of the domain configuration. This value must be unique to a
-- region.
--
-- 'serviceType', 'domainConfigurationSummary_serviceType' - The type of service delivered by the endpoint.
newDomainConfigurationSummary ::
  DomainConfigurationSummary
newDomainConfigurationSummary =
  DomainConfigurationSummary'
    { domainConfigurationArn =
        Core.Nothing,
      domainConfigurationName = Core.Nothing,
      serviceType = Core.Nothing
    }

-- | The ARN of the domain configuration.
domainConfigurationSummary_domainConfigurationArn :: Lens.Lens' DomainConfigurationSummary (Core.Maybe Core.Text)
domainConfigurationSummary_domainConfigurationArn = Lens.lens (\DomainConfigurationSummary' {domainConfigurationArn} -> domainConfigurationArn) (\s@DomainConfigurationSummary' {} a -> s {domainConfigurationArn = a} :: DomainConfigurationSummary)

-- | The name of the domain configuration. This value must be unique to a
-- region.
domainConfigurationSummary_domainConfigurationName :: Lens.Lens' DomainConfigurationSummary (Core.Maybe Core.Text)
domainConfigurationSummary_domainConfigurationName = Lens.lens (\DomainConfigurationSummary' {domainConfigurationName} -> domainConfigurationName) (\s@DomainConfigurationSummary' {} a -> s {domainConfigurationName = a} :: DomainConfigurationSummary)

-- | The type of service delivered by the endpoint.
domainConfigurationSummary_serviceType :: Lens.Lens' DomainConfigurationSummary (Core.Maybe ServiceType)
domainConfigurationSummary_serviceType = Lens.lens (\DomainConfigurationSummary' {serviceType} -> serviceType) (\s@DomainConfigurationSummary' {} a -> s {serviceType = a} :: DomainConfigurationSummary)

instance Core.FromJSON DomainConfigurationSummary where
  parseJSON =
    Core.withObject
      "DomainConfigurationSummary"
      ( \x ->
          DomainConfigurationSummary'
            Core.<$> (x Core..:? "domainConfigurationArn")
            Core.<*> (x Core..:? "domainConfigurationName")
            Core.<*> (x Core..:? "serviceType")
      )

instance Core.Hashable DomainConfigurationSummary

instance Core.NFData DomainConfigurationSummary
