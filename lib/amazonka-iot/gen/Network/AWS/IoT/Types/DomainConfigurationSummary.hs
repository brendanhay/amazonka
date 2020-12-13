{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainConfigurationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DomainConfigurationSummary
  ( DomainConfigurationSummary (..),

    -- * Smart constructor
    mkDomainConfigurationSummary,

    -- * Lenses
    dcsDomainConfigurationName,
    dcsDomainConfigurationARN,
    dcsServiceType,
  )
where

import Network.AWS.IoT.Types.ServiceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary of a domain configuration. A domain configuration specifies custom IoT-specific information about a domain. A domain configuration can be associated with an AWS-managed domain (for example, dbc123defghijk.iot.us-west-2.amazonaws.com), a customer managed domain, or a default endpoint.
--
--
--     * Data
--
--
--     * Jobs
--
--
--     * CredentialProvider
--
--
--
-- /See:/ 'mkDomainConfigurationSummary' smart constructor.
data DomainConfigurationSummary = DomainConfigurationSummary'
  { -- | The name of the domain configuration. This value must be unique to a region.
    domainConfigurationName :: Lude.Maybe Lude.Text,
    -- | The ARN of the domain configuration.
    domainConfigurationARN :: Lude.Maybe Lude.Text,
    -- | The type of service delivered by the endpoint.
    serviceType :: Lude.Maybe ServiceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainConfigurationSummary' with the minimum fields required to make a request.
--
-- * 'domainConfigurationName' - The name of the domain configuration. This value must be unique to a region.
-- * 'domainConfigurationARN' - The ARN of the domain configuration.
-- * 'serviceType' - The type of service delivered by the endpoint.
mkDomainConfigurationSummary ::
  DomainConfigurationSummary
mkDomainConfigurationSummary =
  DomainConfigurationSummary'
    { domainConfigurationName =
        Lude.Nothing,
      domainConfigurationARN = Lude.Nothing,
      serviceType = Lude.Nothing
    }

-- | The name of the domain configuration. This value must be unique to a region.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDomainConfigurationName :: Lens.Lens' DomainConfigurationSummary (Lude.Maybe Lude.Text)
dcsDomainConfigurationName = Lens.lens (domainConfigurationName :: DomainConfigurationSummary -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationName = a} :: DomainConfigurationSummary)
{-# DEPRECATED dcsDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | The ARN of the domain configuration.
--
-- /Note:/ Consider using 'domainConfigurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDomainConfigurationARN :: Lens.Lens' DomainConfigurationSummary (Lude.Maybe Lude.Text)
dcsDomainConfigurationARN = Lens.lens (domainConfigurationARN :: DomainConfigurationSummary -> Lude.Maybe Lude.Text) (\s a -> s {domainConfigurationARN = a} :: DomainConfigurationSummary)
{-# DEPRECATED dcsDomainConfigurationARN "Use generic-lens or generic-optics with 'domainConfigurationARN' instead." #-}

-- | The type of service delivered by the endpoint.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsServiceType :: Lens.Lens' DomainConfigurationSummary (Lude.Maybe ServiceType)
dcsServiceType = Lens.lens (serviceType :: DomainConfigurationSummary -> Lude.Maybe ServiceType) (\s a -> s {serviceType = a} :: DomainConfigurationSummary)
{-# DEPRECATED dcsServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

instance Lude.FromJSON DomainConfigurationSummary where
  parseJSON =
    Lude.withObject
      "DomainConfigurationSummary"
      ( \x ->
          DomainConfigurationSummary'
            Lude.<$> (x Lude..:? "domainConfigurationName")
            Lude.<*> (x Lude..:? "domainConfigurationArn")
            Lude.<*> (x Lude..:? "serviceType")
      )
