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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastDomainOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastDomainOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AutoAcceptSharedAssociationsValue
import Amazonka.EC2.Types.Igmpv2SupportValue
import Amazonka.EC2.Types.StaticSourcesSupportValue
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for a transit gateway multicast domain.
--
-- /See:/ 'newTransitGatewayMulticastDomainOptions' smart constructor.
data TransitGatewayMulticastDomainOptions = TransitGatewayMulticastDomainOptions'
  { -- | Indicates whether support for statically configuring transit gateway
    -- multicast group sources is turned on.
    staticSourcesSupport :: Prelude.Maybe StaticSourcesSupportValue,
    -- | Indicates whether Internet Group Management Protocol (IGMP) version 2 is
    -- turned on for the transit gateway multicast domain.
    igmpv2Support :: Prelude.Maybe Igmpv2SupportValue,
    -- | Indicates whether to automatically cross-account subnet associations
    -- that are associated with the transit gateway multicast domain.
    autoAcceptSharedAssociations :: Prelude.Maybe AutoAcceptSharedAssociationsValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDomainOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticSourcesSupport', 'transitGatewayMulticastDomainOptions_staticSourcesSupport' - Indicates whether support for statically configuring transit gateway
-- multicast group sources is turned on.
--
-- 'igmpv2Support', 'transitGatewayMulticastDomainOptions_igmpv2Support' - Indicates whether Internet Group Management Protocol (IGMP) version 2 is
-- turned on for the transit gateway multicast domain.
--
-- 'autoAcceptSharedAssociations', 'transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations' - Indicates whether to automatically cross-account subnet associations
-- that are associated with the transit gateway multicast domain.
newTransitGatewayMulticastDomainOptions ::
  TransitGatewayMulticastDomainOptions
newTransitGatewayMulticastDomainOptions =
  TransitGatewayMulticastDomainOptions'
    { staticSourcesSupport =
        Prelude.Nothing,
      igmpv2Support = Prelude.Nothing,
      autoAcceptSharedAssociations =
        Prelude.Nothing
    }

-- | Indicates whether support for statically configuring transit gateway
-- multicast group sources is turned on.
transitGatewayMulticastDomainOptions_staticSourcesSupport :: Lens.Lens' TransitGatewayMulticastDomainOptions (Prelude.Maybe StaticSourcesSupportValue)
transitGatewayMulticastDomainOptions_staticSourcesSupport = Lens.lens (\TransitGatewayMulticastDomainOptions' {staticSourcesSupport} -> staticSourcesSupport) (\s@TransitGatewayMulticastDomainOptions' {} a -> s {staticSourcesSupport = a} :: TransitGatewayMulticastDomainOptions)

-- | Indicates whether Internet Group Management Protocol (IGMP) version 2 is
-- turned on for the transit gateway multicast domain.
transitGatewayMulticastDomainOptions_igmpv2Support :: Lens.Lens' TransitGatewayMulticastDomainOptions (Prelude.Maybe Igmpv2SupportValue)
transitGatewayMulticastDomainOptions_igmpv2Support = Lens.lens (\TransitGatewayMulticastDomainOptions' {igmpv2Support} -> igmpv2Support) (\s@TransitGatewayMulticastDomainOptions' {} a -> s {igmpv2Support = a} :: TransitGatewayMulticastDomainOptions)

-- | Indicates whether to automatically cross-account subnet associations
-- that are associated with the transit gateway multicast domain.
transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations :: Lens.Lens' TransitGatewayMulticastDomainOptions (Prelude.Maybe AutoAcceptSharedAssociationsValue)
transitGatewayMulticastDomainOptions_autoAcceptSharedAssociations = Lens.lens (\TransitGatewayMulticastDomainOptions' {autoAcceptSharedAssociations} -> autoAcceptSharedAssociations) (\s@TransitGatewayMulticastDomainOptions' {} a -> s {autoAcceptSharedAssociations = a} :: TransitGatewayMulticastDomainOptions)

instance
  Core.FromXML
    TransitGatewayMulticastDomainOptions
  where
  parseXML x =
    TransitGatewayMulticastDomainOptions'
      Prelude.<$> (x Core..@? "staticSourcesSupport")
      Prelude.<*> (x Core..@? "igmpv2Support")
      Prelude.<*> (x Core..@? "autoAcceptSharedAssociations")

instance
  Prelude.Hashable
    TransitGatewayMulticastDomainOptions
  where
  hashWithSalt
    _salt
    TransitGatewayMulticastDomainOptions' {..} =
      _salt `Prelude.hashWithSalt` staticSourcesSupport
        `Prelude.hashWithSalt` igmpv2Support
        `Prelude.hashWithSalt` autoAcceptSharedAssociations

instance
  Prelude.NFData
    TransitGatewayMulticastDomainOptions
  where
  rnf TransitGatewayMulticastDomainOptions' {..} =
    Prelude.rnf staticSourcesSupport
      `Prelude.seq` Prelude.rnf igmpv2Support
      `Prelude.seq` Prelude.rnf autoAcceptSharedAssociations
