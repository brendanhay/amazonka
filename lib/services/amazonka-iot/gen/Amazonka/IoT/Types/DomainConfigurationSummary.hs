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
-- Module      : Amazonka.IoT.Types.DomainConfigurationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DomainConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ServiceType
import qualified Amazonka.Prelude as Prelude

-- | The summary of a domain configuration. A domain configuration specifies
-- custom IoT-specific information about a domain. A domain configuration
-- can be associated with an Amazon Web Services-managed domain (for
-- example, dbc123defghijk.iot.us-west-2.amazonaws.com), a customer managed
-- domain, or a default endpoint.
--
-- -   Data
--
-- -   Jobs
--
-- -   CredentialProvider
--
-- /See:/ 'newDomainConfigurationSummary' smart constructor.
data DomainConfigurationSummary = DomainConfigurationSummary'
  { -- | The ARN of the domain configuration.
    domainConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The type of service delivered by the endpoint.
    serviceType :: Prelude.Maybe ServiceType,
    -- | The name of the domain configuration. This value must be unique to a
    -- region.
    domainConfigurationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'serviceType', 'domainConfigurationSummary_serviceType' - The type of service delivered by the endpoint.
--
-- 'domainConfigurationName', 'domainConfigurationSummary_domainConfigurationName' - The name of the domain configuration. This value must be unique to a
-- region.
newDomainConfigurationSummary ::
  DomainConfigurationSummary
newDomainConfigurationSummary =
  DomainConfigurationSummary'
    { domainConfigurationArn =
        Prelude.Nothing,
      serviceType = Prelude.Nothing,
      domainConfigurationName = Prelude.Nothing
    }

-- | The ARN of the domain configuration.
domainConfigurationSummary_domainConfigurationArn :: Lens.Lens' DomainConfigurationSummary (Prelude.Maybe Prelude.Text)
domainConfigurationSummary_domainConfigurationArn = Lens.lens (\DomainConfigurationSummary' {domainConfigurationArn} -> domainConfigurationArn) (\s@DomainConfigurationSummary' {} a -> s {domainConfigurationArn = a} :: DomainConfigurationSummary)

-- | The type of service delivered by the endpoint.
domainConfigurationSummary_serviceType :: Lens.Lens' DomainConfigurationSummary (Prelude.Maybe ServiceType)
domainConfigurationSummary_serviceType = Lens.lens (\DomainConfigurationSummary' {serviceType} -> serviceType) (\s@DomainConfigurationSummary' {} a -> s {serviceType = a} :: DomainConfigurationSummary)

-- | The name of the domain configuration. This value must be unique to a
-- region.
domainConfigurationSummary_domainConfigurationName :: Lens.Lens' DomainConfigurationSummary (Prelude.Maybe Prelude.Text)
domainConfigurationSummary_domainConfigurationName = Lens.lens (\DomainConfigurationSummary' {domainConfigurationName} -> domainConfigurationName) (\s@DomainConfigurationSummary' {} a -> s {domainConfigurationName = a} :: DomainConfigurationSummary)

instance Data.FromJSON DomainConfigurationSummary where
  parseJSON =
    Data.withObject
      "DomainConfigurationSummary"
      ( \x ->
          DomainConfigurationSummary'
            Prelude.<$> (x Data..:? "domainConfigurationArn")
            Prelude.<*> (x Data..:? "serviceType")
            Prelude.<*> (x Data..:? "domainConfigurationName")
      )

instance Prelude.Hashable DomainConfigurationSummary where
  hashWithSalt _salt DomainConfigurationSummary' {..} =
    _salt `Prelude.hashWithSalt` domainConfigurationArn
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` domainConfigurationName

instance Prelude.NFData DomainConfigurationSummary where
  rnf DomainConfigurationSummary' {..} =
    Prelude.rnf domainConfigurationArn
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf domainConfigurationName
