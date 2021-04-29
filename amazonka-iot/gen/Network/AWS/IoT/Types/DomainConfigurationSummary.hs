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
-- Module      : Network.AWS.IoT.Types.DomainConfigurationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DomainConfigurationSummary where

import Network.AWS.IoT.Types.ServiceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    domainConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain configuration. This value must be unique to a
    -- region.
    domainConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The type of service delivered by the endpoint.
    serviceType :: Prelude.Maybe ServiceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      domainConfigurationName = Prelude.Nothing,
      serviceType = Prelude.Nothing
    }

-- | The ARN of the domain configuration.
domainConfigurationSummary_domainConfigurationArn :: Lens.Lens' DomainConfigurationSummary (Prelude.Maybe Prelude.Text)
domainConfigurationSummary_domainConfigurationArn = Lens.lens (\DomainConfigurationSummary' {domainConfigurationArn} -> domainConfigurationArn) (\s@DomainConfigurationSummary' {} a -> s {domainConfigurationArn = a} :: DomainConfigurationSummary)

-- | The name of the domain configuration. This value must be unique to a
-- region.
domainConfigurationSummary_domainConfigurationName :: Lens.Lens' DomainConfigurationSummary (Prelude.Maybe Prelude.Text)
domainConfigurationSummary_domainConfigurationName = Lens.lens (\DomainConfigurationSummary' {domainConfigurationName} -> domainConfigurationName) (\s@DomainConfigurationSummary' {} a -> s {domainConfigurationName = a} :: DomainConfigurationSummary)

-- | The type of service delivered by the endpoint.
domainConfigurationSummary_serviceType :: Lens.Lens' DomainConfigurationSummary (Prelude.Maybe ServiceType)
domainConfigurationSummary_serviceType = Lens.lens (\DomainConfigurationSummary' {serviceType} -> serviceType) (\s@DomainConfigurationSummary' {} a -> s {serviceType = a} :: DomainConfigurationSummary)

instance Prelude.FromJSON DomainConfigurationSummary where
  parseJSON =
    Prelude.withObject
      "DomainConfigurationSummary"
      ( \x ->
          DomainConfigurationSummary'
            Prelude.<$> (x Prelude..:? "domainConfigurationArn")
            Prelude.<*> (x Prelude..:? "domainConfigurationName")
            Prelude.<*> (x Prelude..:? "serviceType")
      )

instance Prelude.Hashable DomainConfigurationSummary

instance Prelude.NFData DomainConfigurationSummary
