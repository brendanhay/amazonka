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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomain where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainOptions
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
import qualified Network.AWS.Lens as Lens

-- | Describes the transit gateway multicast domain.
--
-- /See:/ 'newTransitGatewayMulticastDomain' smart constructor.
data TransitGatewayMulticastDomain = TransitGatewayMulticastDomain'
  { -- | The ID of the AWS account that owns the transit gateway multiicast
    -- domain.
    ownerId :: Core.Maybe Core.Text,
    -- | The time the transit gateway multicast domain was created.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The options for the transit gateway multicast domain.
    options :: Core.Maybe TransitGatewayMulticastDomainOptions,
    -- | The state of the transit gateway multicast domain.
    state :: Core.Maybe TransitGatewayMulticastDomainState,
    -- | The tags for the transit gateway multicast domain.
    tags :: Core.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the transit gateway multicast domain.
    transitGatewayMulticastDomainArn :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'transitGatewayMulticastDomain_ownerId' - The ID of the AWS account that owns the transit gateway multiicast
-- domain.
--
-- 'creationTime', 'transitGatewayMulticastDomain_creationTime' - The time the transit gateway multicast domain was created.
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastDomain_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'options', 'transitGatewayMulticastDomain_options' - The options for the transit gateway multicast domain.
--
-- 'state', 'transitGatewayMulticastDomain_state' - The state of the transit gateway multicast domain.
--
-- 'tags', 'transitGatewayMulticastDomain_tags' - The tags for the transit gateway multicast domain.
--
-- 'transitGatewayMulticastDomainArn', 'transitGatewayMulticastDomain_transitGatewayMulticastDomainArn' - The Amazon Resource Name (ARN) of the transit gateway multicast domain.
--
-- 'transitGatewayId', 'transitGatewayMulticastDomain_transitGatewayId' - The ID of the transit gateway.
newTransitGatewayMulticastDomain ::
  TransitGatewayMulticastDomain
newTransitGatewayMulticastDomain =
  TransitGatewayMulticastDomain'
    { ownerId =
        Core.Nothing,
      creationTime = Core.Nothing,
      transitGatewayMulticastDomainId =
        Core.Nothing,
      options = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      transitGatewayMulticastDomainArn =
        Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The ID of the AWS account that owns the transit gateway multiicast
-- domain.
transitGatewayMulticastDomain_ownerId :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Core.Text)
transitGatewayMulticastDomain_ownerId = Lens.lens (\TransitGatewayMulticastDomain' {ownerId} -> ownerId) (\s@TransitGatewayMulticastDomain' {} a -> s {ownerId = a} :: TransitGatewayMulticastDomain)

-- | The time the transit gateway multicast domain was created.
transitGatewayMulticastDomain_creationTime :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Core.UTCTime)
transitGatewayMulticastDomain_creationTime = Lens.lens (\TransitGatewayMulticastDomain' {creationTime} -> creationTime) (\s@TransitGatewayMulticastDomain' {} a -> s {creationTime = a} :: TransitGatewayMulticastDomain) Core.. Lens.mapping Core._Time

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Core.Text)
transitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDomain)

-- | The options for the transit gateway multicast domain.
transitGatewayMulticastDomain_options :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe TransitGatewayMulticastDomainOptions)
transitGatewayMulticastDomain_options = Lens.lens (\TransitGatewayMulticastDomain' {options} -> options) (\s@TransitGatewayMulticastDomain' {} a -> s {options = a} :: TransitGatewayMulticastDomain)

-- | The state of the transit gateway multicast domain.
transitGatewayMulticastDomain_state :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe TransitGatewayMulticastDomainState)
transitGatewayMulticastDomain_state = Lens.lens (\TransitGatewayMulticastDomain' {state} -> state) (\s@TransitGatewayMulticastDomain' {} a -> s {state = a} :: TransitGatewayMulticastDomain)

-- | The tags for the transit gateway multicast domain.
transitGatewayMulticastDomain_tags :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe [Tag])
transitGatewayMulticastDomain_tags = Lens.lens (\TransitGatewayMulticastDomain' {tags} -> tags) (\s@TransitGatewayMulticastDomain' {} a -> s {tags = a} :: TransitGatewayMulticastDomain) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the transit gateway multicast domain.
transitGatewayMulticastDomain_transitGatewayMulticastDomainArn :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Core.Text)
transitGatewayMulticastDomain_transitGatewayMulticastDomainArn = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayMulticastDomainArn} -> transitGatewayMulticastDomainArn) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainArn = a} :: TransitGatewayMulticastDomain)

-- | The ID of the transit gateway.
transitGatewayMulticastDomain_transitGatewayId :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Core.Text)
transitGatewayMulticastDomain_transitGatewayId = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayId = a} :: TransitGatewayMulticastDomain)

instance Core.FromXML TransitGatewayMulticastDomain where
  parseXML x =
    TransitGatewayMulticastDomain'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "transitGatewayMulticastDomainId")
      Core.<*> (x Core..@? "options")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "transitGatewayMulticastDomainArn")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable TransitGatewayMulticastDomain

instance Core.NFData TransitGatewayMulticastDomain
