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
-- Module      : Network.AWS.EC2.Types.TransitGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayOptions
import Network.AWS.EC2.Types.TransitGatewayState
import qualified Network.AWS.Lens as Lens

-- | Describes a transit gateway.
--
-- /See:/ 'newTransitGateway' smart constructor.
data TransitGateway = TransitGateway'
  { -- | The ID of the AWS account ID that owns the transit gateway.
    ownerId :: Core.Maybe Core.Text,
    -- | The creation time.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | The transit gateway options.
    options :: Core.Maybe TransitGatewayOptions,
    -- | The Amazon Resource Name (ARN) of the transit gateway.
    transitGatewayArn :: Core.Maybe Core.Text,
    -- | The state of the transit gateway.
    state :: Core.Maybe TransitGatewayState,
    -- | The tags for the transit gateway.
    tags :: Core.Maybe [Tag],
    -- | The description of the transit gateway.
    description :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'transitGateway_ownerId' - The ID of the AWS account ID that owns the transit gateway.
--
-- 'creationTime', 'transitGateway_creationTime' - The creation time.
--
-- 'options', 'transitGateway_options' - The transit gateway options.
--
-- 'transitGatewayArn', 'transitGateway_transitGatewayArn' - The Amazon Resource Name (ARN) of the transit gateway.
--
-- 'state', 'transitGateway_state' - The state of the transit gateway.
--
-- 'tags', 'transitGateway_tags' - The tags for the transit gateway.
--
-- 'description', 'transitGateway_description' - The description of the transit gateway.
--
-- 'transitGatewayId', 'transitGateway_transitGatewayId' - The ID of the transit gateway.
newTransitGateway ::
  TransitGateway
newTransitGateway =
  TransitGateway'
    { ownerId = Core.Nothing,
      creationTime = Core.Nothing,
      options = Core.Nothing,
      transitGatewayArn = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The ID of the AWS account ID that owns the transit gateway.
transitGateway_ownerId :: Lens.Lens' TransitGateway (Core.Maybe Core.Text)
transitGateway_ownerId = Lens.lens (\TransitGateway' {ownerId} -> ownerId) (\s@TransitGateway' {} a -> s {ownerId = a} :: TransitGateway)

-- | The creation time.
transitGateway_creationTime :: Lens.Lens' TransitGateway (Core.Maybe Core.UTCTime)
transitGateway_creationTime = Lens.lens (\TransitGateway' {creationTime} -> creationTime) (\s@TransitGateway' {} a -> s {creationTime = a} :: TransitGateway) Core.. Lens.mapping Core._Time

-- | The transit gateway options.
transitGateway_options :: Lens.Lens' TransitGateway (Core.Maybe TransitGatewayOptions)
transitGateway_options = Lens.lens (\TransitGateway' {options} -> options) (\s@TransitGateway' {} a -> s {options = a} :: TransitGateway)

-- | The Amazon Resource Name (ARN) of the transit gateway.
transitGateway_transitGatewayArn :: Lens.Lens' TransitGateway (Core.Maybe Core.Text)
transitGateway_transitGatewayArn = Lens.lens (\TransitGateway' {transitGatewayArn} -> transitGatewayArn) (\s@TransitGateway' {} a -> s {transitGatewayArn = a} :: TransitGateway)

-- | The state of the transit gateway.
transitGateway_state :: Lens.Lens' TransitGateway (Core.Maybe TransitGatewayState)
transitGateway_state = Lens.lens (\TransitGateway' {state} -> state) (\s@TransitGateway' {} a -> s {state = a} :: TransitGateway)

-- | The tags for the transit gateway.
transitGateway_tags :: Lens.Lens' TransitGateway (Core.Maybe [Tag])
transitGateway_tags = Lens.lens (\TransitGateway' {tags} -> tags) (\s@TransitGateway' {} a -> s {tags = a} :: TransitGateway) Core.. Lens.mapping Lens._Coerce

-- | The description of the transit gateway.
transitGateway_description :: Lens.Lens' TransitGateway (Core.Maybe Core.Text)
transitGateway_description = Lens.lens (\TransitGateway' {description} -> description) (\s@TransitGateway' {} a -> s {description = a} :: TransitGateway)

-- | The ID of the transit gateway.
transitGateway_transitGatewayId :: Lens.Lens' TransitGateway (Core.Maybe Core.Text)
transitGateway_transitGatewayId = Lens.lens (\TransitGateway' {transitGatewayId} -> transitGatewayId) (\s@TransitGateway' {} a -> s {transitGatewayId = a} :: TransitGateway)

instance Core.FromXML TransitGateway where
  parseXML x =
    TransitGateway'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "options")
      Core.<*> (x Core..@? "transitGatewayArn")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable TransitGateway

instance Core.NFData TransitGateway
