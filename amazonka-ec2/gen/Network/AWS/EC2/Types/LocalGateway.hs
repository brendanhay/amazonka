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
-- Module      : Network.AWS.EC2.Types.LocalGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a local gateway.
--
-- /See:/ 'newLocalGateway' smart constructor.
data LocalGateway = LocalGateway'
  { -- | The AWS account ID that owns the local gateway.
    ownerId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Core.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Core.Text,
    -- | The state of the local gateway.
    state :: Core.Maybe Core.Text,
    -- | The tags assigned to the local gateway.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LocalGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'localGateway_ownerId' - The AWS account ID that owns the local gateway.
--
-- 'outpostArn', 'localGateway_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'localGatewayId', 'localGateway_localGatewayId' - The ID of the local gateway.
--
-- 'state', 'localGateway_state' - The state of the local gateway.
--
-- 'tags', 'localGateway_tags' - The tags assigned to the local gateway.
newLocalGateway ::
  LocalGateway
newLocalGateway =
  LocalGateway'
    { ownerId = Core.Nothing,
      outpostArn = Core.Nothing,
      localGatewayId = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing
    }

-- | The AWS account ID that owns the local gateway.
localGateway_ownerId :: Lens.Lens' LocalGateway (Core.Maybe Core.Text)
localGateway_ownerId = Lens.lens (\LocalGateway' {ownerId} -> ownerId) (\s@LocalGateway' {} a -> s {ownerId = a} :: LocalGateway)

-- | The Amazon Resource Name (ARN) of the Outpost.
localGateway_outpostArn :: Lens.Lens' LocalGateway (Core.Maybe Core.Text)
localGateway_outpostArn = Lens.lens (\LocalGateway' {outpostArn} -> outpostArn) (\s@LocalGateway' {} a -> s {outpostArn = a} :: LocalGateway)

-- | The ID of the local gateway.
localGateway_localGatewayId :: Lens.Lens' LocalGateway (Core.Maybe Core.Text)
localGateway_localGatewayId = Lens.lens (\LocalGateway' {localGatewayId} -> localGatewayId) (\s@LocalGateway' {} a -> s {localGatewayId = a} :: LocalGateway)

-- | The state of the local gateway.
localGateway_state :: Lens.Lens' LocalGateway (Core.Maybe Core.Text)
localGateway_state = Lens.lens (\LocalGateway' {state} -> state) (\s@LocalGateway' {} a -> s {state = a} :: LocalGateway)

-- | The tags assigned to the local gateway.
localGateway_tags :: Lens.Lens' LocalGateway (Core.Maybe [Tag])
localGateway_tags = Lens.lens (\LocalGateway' {tags} -> tags) (\s@LocalGateway' {} a -> s {tags = a} :: LocalGateway) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML LocalGateway where
  parseXML x =
    LocalGateway'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "localGatewayId")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable LocalGateway

instance Core.NFData LocalGateway
