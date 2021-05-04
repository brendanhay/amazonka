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
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomain where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainOptions
import Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the transit gateway multicast domain.
--
-- /See:/ 'newTransitGatewayMulticastDomain' smart constructor.
data TransitGatewayMulticastDomain = TransitGatewayMulticastDomain'
  { -- | The ID of the AWS account that owns the transit gateway multiicast
    -- domain.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The time the transit gateway multicast domain was created.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The options for the transit gateway multicast domain.
    options :: Prelude.Maybe TransitGatewayMulticastDomainOptions,
    -- | The state of the transit gateway multicast domain.
    state :: Prelude.Maybe TransitGatewayMulticastDomainState,
    -- | The tags for the transit gateway multicast domain.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the transit gateway multicast domain.
    transitGatewayMulticastDomainArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing,
      options = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayMulticastDomainArn =
        Prelude.Nothing,
      transitGatewayId = Prelude.Nothing
    }

-- | The ID of the AWS account that owns the transit gateway multiicast
-- domain.
transitGatewayMulticastDomain_ownerId :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_ownerId = Lens.lens (\TransitGatewayMulticastDomain' {ownerId} -> ownerId) (\s@TransitGatewayMulticastDomain' {} a -> s {ownerId = a} :: TransitGatewayMulticastDomain)

-- | The time the transit gateway multicast domain was created.
transitGatewayMulticastDomain_creationTime :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.UTCTime)
transitGatewayMulticastDomain_creationTime = Lens.lens (\TransitGatewayMulticastDomain' {creationTime} -> creationTime) (\s@TransitGatewayMulticastDomain' {} a -> s {creationTime = a} :: TransitGatewayMulticastDomain) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDomain)

-- | The options for the transit gateway multicast domain.
transitGatewayMulticastDomain_options :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe TransitGatewayMulticastDomainOptions)
transitGatewayMulticastDomain_options = Lens.lens (\TransitGatewayMulticastDomain' {options} -> options) (\s@TransitGatewayMulticastDomain' {} a -> s {options = a} :: TransitGatewayMulticastDomain)

-- | The state of the transit gateway multicast domain.
transitGatewayMulticastDomain_state :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe TransitGatewayMulticastDomainState)
transitGatewayMulticastDomain_state = Lens.lens (\TransitGatewayMulticastDomain' {state} -> state) (\s@TransitGatewayMulticastDomain' {} a -> s {state = a} :: TransitGatewayMulticastDomain)

-- | The tags for the transit gateway multicast domain.
transitGatewayMulticastDomain_tags :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe [Tag])
transitGatewayMulticastDomain_tags = Lens.lens (\TransitGatewayMulticastDomain' {tags} -> tags) (\s@TransitGatewayMulticastDomain' {} a -> s {tags = a} :: TransitGatewayMulticastDomain) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the transit gateway multicast domain.
transitGatewayMulticastDomain_transitGatewayMulticastDomainArn :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_transitGatewayMulticastDomainArn = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayMulticastDomainArn} -> transitGatewayMulticastDomainArn) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainArn = a} :: TransitGatewayMulticastDomain)

-- | The ID of the transit gateway.
transitGatewayMulticastDomain_transitGatewayId :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_transitGatewayId = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayId = a} :: TransitGatewayMulticastDomain)

instance
  Prelude.FromXML
    TransitGatewayMulticastDomain
  where
  parseXML x =
    TransitGatewayMulticastDomain'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "transitGatewayMulticastDomainId")
      Prelude.<*> (x Prelude..@? "options")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "transitGatewayMulticastDomainArn")
      Prelude.<*> (x Prelude..@? "transitGatewayId")

instance
  Prelude.Hashable
    TransitGatewayMulticastDomain

instance Prelude.NFData TransitGatewayMulticastDomain
