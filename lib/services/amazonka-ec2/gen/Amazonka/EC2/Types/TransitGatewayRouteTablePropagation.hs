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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteTablePropagation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import Amazonka.EC2.Types.TransitGatewayPropagationState
import qualified Amazonka.Prelude as Prelude

-- | Describes a route table propagation.
--
-- /See:/ 'newTransitGatewayRouteTablePropagation' smart constructor.
data TransitGatewayRouteTablePropagation = TransitGatewayRouteTablePropagation'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The state of the resource.
    state :: Prelude.Maybe TransitGatewayPropagationState,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table announcement.
    transitGatewayRouteTableAnnouncementId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRouteTablePropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayRouteTablePropagation_resourceId' - The ID of the resource.
--
-- 'resourceType', 'transitGatewayRouteTablePropagation_resourceType' - The type of resource. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'state', 'transitGatewayRouteTablePropagation_state' - The state of the resource.
--
-- 'transitGatewayAttachmentId', 'transitGatewayRouteTablePropagation_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'transitGatewayRouteTableAnnouncementId', 'transitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId' - The ID of the transit gateway route table announcement.
newTransitGatewayRouteTablePropagation ::
  TransitGatewayRouteTablePropagation
newTransitGatewayRouteTablePropagation =
  TransitGatewayRouteTablePropagation'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing,
      transitGatewayRouteTableAnnouncementId =
        Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayRouteTablePropagation_resourceId :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Text)
transitGatewayRouteTablePropagation_resourceId = Lens.lens (\TransitGatewayRouteTablePropagation' {resourceId} -> resourceId) (\s@TransitGatewayRouteTablePropagation' {} a -> s {resourceId = a} :: TransitGatewayRouteTablePropagation)

-- | The type of resource. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayRouteTablePropagation_resourceType :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayRouteTablePropagation_resourceType = Lens.lens (\TransitGatewayRouteTablePropagation' {resourceType} -> resourceType) (\s@TransitGatewayRouteTablePropagation' {} a -> s {resourceType = a} :: TransitGatewayRouteTablePropagation)

-- | The state of the resource.
transitGatewayRouteTablePropagation_state :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe TransitGatewayPropagationState)
transitGatewayRouteTablePropagation_state = Lens.lens (\TransitGatewayRouteTablePropagation' {state} -> state) (\s@TransitGatewayRouteTablePropagation' {} a -> s {state = a} :: TransitGatewayRouteTablePropagation)

-- | The ID of the attachment.
transitGatewayRouteTablePropagation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Text)
transitGatewayRouteTablePropagation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayRouteTablePropagation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayRouteTablePropagation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayRouteTablePropagation)

-- | The ID of the transit gateway route table announcement.
transitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Text)
transitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId = Lens.lens (\TransitGatewayRouteTablePropagation' {transitGatewayRouteTableAnnouncementId} -> transitGatewayRouteTableAnnouncementId) (\s@TransitGatewayRouteTablePropagation' {} a -> s {transitGatewayRouteTableAnnouncementId = a} :: TransitGatewayRouteTablePropagation)

instance
  Data.FromXML
    TransitGatewayRouteTablePropagation
  where
  parseXML x =
    TransitGatewayRouteTablePropagation'
      Prelude.<$> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Data..@? "transitGatewayRouteTableAnnouncementId")

instance
  Prelude.Hashable
    TransitGatewayRouteTablePropagation
  where
  hashWithSalt
    _salt
    TransitGatewayRouteTablePropagation' {..} =
      _salt `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayRouteTableAnnouncementId

instance
  Prelude.NFData
    TransitGatewayRouteTablePropagation
  where
  rnf TransitGatewayRouteTablePropagation' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableAnnouncementId
