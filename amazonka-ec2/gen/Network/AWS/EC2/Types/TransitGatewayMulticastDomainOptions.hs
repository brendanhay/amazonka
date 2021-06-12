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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AutoAcceptSharedAssociationsValue
import Network.AWS.EC2.Types.Igmpv2SupportValue
import Network.AWS.EC2.Types.StaticSourcesSupportValue
import qualified Network.AWS.Lens as Lens

-- | Describes the options for a transit gateway multicast domain.
--
-- /See:/ 'newTransitGatewayMulticastDomainOptions' smart constructor.
data TransitGatewayMulticastDomainOptions = TransitGatewayMulticastDomainOptions'
  { -- | Indicates whether Internet Group Management Protocol (IGMP) version 2 is
    -- turned on for the transit gateway multicast domain.
    igmpv2Support :: Core.Maybe Igmpv2SupportValue,
    -- | Indicates whether to automatically cross-account subnet associations
    -- that are associated with the transit gateway multicast domain.
    autoAcceptSharedAssociations :: Core.Maybe AutoAcceptSharedAssociationsValue,
    -- | Indicates whether support for statically configuring transit gateway
    -- multicast group sources is turned on.
    staticSourcesSupport :: Core.Maybe StaticSourcesSupportValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDomainOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'igmpv2Support', 'transitGatewayMulticastDomainOptions_igmpv2Support' - Indicates whether Internet Group Management Protocol (IGMP) version 2 is
-- turned on for the transit gateway multicast domain.
--
-- 'autoAcceptSharedAssociations', 'transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations' - Indicates whether to automatically cross-account subnet associations
-- that are associated with the transit gateway multicast domain.
--
-- 'staticSourcesSupport', 'transitGatewayMulticastDomainOptions_staticSourcesSupport' - Indicates whether support for statically configuring transit gateway
-- multicast group sources is turned on.
newTransitGatewayMulticastDomainOptions ::
  TransitGatewayMulticastDomainOptions
newTransitGatewayMulticastDomainOptions =
  TransitGatewayMulticastDomainOptions'
    { igmpv2Support =
        Core.Nothing,
      autoAcceptSharedAssociations =
        Core.Nothing,
      staticSourcesSupport = Core.Nothing
    }

-- | Indicates whether Internet Group Management Protocol (IGMP) version 2 is
-- turned on for the transit gateway multicast domain.
transitGatewayMulticastDomainOptions_igmpv2Support :: Lens.Lens' TransitGatewayMulticastDomainOptions (Core.Maybe Igmpv2SupportValue)
transitGatewayMulticastDomainOptions_igmpv2Support = Lens.lens (\TransitGatewayMulticastDomainOptions' {igmpv2Support} -> igmpv2Support) (\s@TransitGatewayMulticastDomainOptions' {} a -> s {igmpv2Support = a} :: TransitGatewayMulticastDomainOptions)

-- | Indicates whether to automatically cross-account subnet associations
-- that are associated with the transit gateway multicast domain.
transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations :: Lens.Lens' TransitGatewayMulticastDomainOptions (Core.Maybe AutoAcceptSharedAssociationsValue)
transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations = Lens.lens (\TransitGatewayMulticastDomainOptions' {autoAcceptSharedAssociations} -> autoAcceptSharedAssociations) (\s@TransitGatewayMulticastDomainOptions' {} a -> s {autoAcceptSharedAssociations = a} :: TransitGatewayMulticastDomainOptions)

-- | Indicates whether support for statically configuring transit gateway
-- multicast group sources is turned on.
transitGatewayMulticastDomainOptions_staticSourcesSupport :: Lens.Lens' TransitGatewayMulticastDomainOptions (Core.Maybe StaticSourcesSupportValue)
transitGatewayMulticastDomainOptions_staticSourcesSupport = Lens.lens (\TransitGatewayMulticastDomainOptions' {staticSourcesSupport} -> staticSourcesSupport) (\s@TransitGatewayMulticastDomainOptions' {} a -> s {staticSourcesSupport = a} :: TransitGatewayMulticastDomainOptions)

instance
  Core.FromXML
    TransitGatewayMulticastDomainOptions
  where
  parseXML x =
    TransitGatewayMulticastDomainOptions'
      Core.<$> (x Core..@? "igmpv2Support")
      Core.<*> (x Core..@? "autoAcceptSharedAssociations")
      Core.<*> (x Core..@? "staticSourcesSupport")

instance
  Core.Hashable
    TransitGatewayMulticastDomainOptions

instance
  Core.NFData
    TransitGatewayMulticastDomainOptions
