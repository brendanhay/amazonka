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
-- Module      : Amazonka.NetworkManager.Types.NetworkRouteDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.NetworkRouteDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination of a network route.
--
-- /See:/ 'newNetworkRouteDestination' smart constructor.
data NetworkRouteDestination = NetworkRouteDestination'
  { -- | The ID of a core network attachment.
    coreNetworkAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The edge location for the network destination.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the segment.
    segmentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkRouteDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkAttachmentId', 'networkRouteDestination_coreNetworkAttachmentId' - The ID of a core network attachment.
--
-- 'edgeLocation', 'networkRouteDestination_edgeLocation' - The edge location for the network destination.
--
-- 'resourceId', 'networkRouteDestination_resourceId' - The ID of the resource.
--
-- 'resourceType', 'networkRouteDestination_resourceType' - The resource type.
--
-- 'segmentName', 'networkRouteDestination_segmentName' - The name of the segment.
--
-- 'transitGatewayAttachmentId', 'networkRouteDestination_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
newNetworkRouteDestination ::
  NetworkRouteDestination
newNetworkRouteDestination =
  NetworkRouteDestination'
    { coreNetworkAttachmentId =
        Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      segmentName = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing
    }

-- | The ID of a core network attachment.
networkRouteDestination_coreNetworkAttachmentId :: Lens.Lens' NetworkRouteDestination (Prelude.Maybe Prelude.Text)
networkRouteDestination_coreNetworkAttachmentId = Lens.lens (\NetworkRouteDestination' {coreNetworkAttachmentId} -> coreNetworkAttachmentId) (\s@NetworkRouteDestination' {} a -> s {coreNetworkAttachmentId = a} :: NetworkRouteDestination)

-- | The edge location for the network destination.
networkRouteDestination_edgeLocation :: Lens.Lens' NetworkRouteDestination (Prelude.Maybe Prelude.Text)
networkRouteDestination_edgeLocation = Lens.lens (\NetworkRouteDestination' {edgeLocation} -> edgeLocation) (\s@NetworkRouteDestination' {} a -> s {edgeLocation = a} :: NetworkRouteDestination)

-- | The ID of the resource.
networkRouteDestination_resourceId :: Lens.Lens' NetworkRouteDestination (Prelude.Maybe Prelude.Text)
networkRouteDestination_resourceId = Lens.lens (\NetworkRouteDestination' {resourceId} -> resourceId) (\s@NetworkRouteDestination' {} a -> s {resourceId = a} :: NetworkRouteDestination)

-- | The resource type.
networkRouteDestination_resourceType :: Lens.Lens' NetworkRouteDestination (Prelude.Maybe Prelude.Text)
networkRouteDestination_resourceType = Lens.lens (\NetworkRouteDestination' {resourceType} -> resourceType) (\s@NetworkRouteDestination' {} a -> s {resourceType = a} :: NetworkRouteDestination)

-- | The name of the segment.
networkRouteDestination_segmentName :: Lens.Lens' NetworkRouteDestination (Prelude.Maybe Prelude.Text)
networkRouteDestination_segmentName = Lens.lens (\NetworkRouteDestination' {segmentName} -> segmentName) (\s@NetworkRouteDestination' {} a -> s {segmentName = a} :: NetworkRouteDestination)

-- | The ID of the transit gateway attachment.
networkRouteDestination_transitGatewayAttachmentId :: Lens.Lens' NetworkRouteDestination (Prelude.Maybe Prelude.Text)
networkRouteDestination_transitGatewayAttachmentId = Lens.lens (\NetworkRouteDestination' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@NetworkRouteDestination' {} a -> s {transitGatewayAttachmentId = a} :: NetworkRouteDestination)

instance Data.FromJSON NetworkRouteDestination where
  parseJSON =
    Data.withObject
      "NetworkRouteDestination"
      ( \x ->
          NetworkRouteDestination'
            Prelude.<$> (x Data..:? "CoreNetworkAttachmentId")
            Prelude.<*> (x Data..:? "EdgeLocation")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "SegmentName")
            Prelude.<*> (x Data..:? "TransitGatewayAttachmentId")
      )

instance Prelude.Hashable NetworkRouteDestination where
  hashWithSalt _salt NetworkRouteDestination' {..} =
    _salt
      `Prelude.hashWithSalt` coreNetworkAttachmentId
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` segmentName
      `Prelude.hashWithSalt` transitGatewayAttachmentId

instance Prelude.NFData NetworkRouteDestination where
  rnf NetworkRouteDestination' {..} =
    Prelude.rnf coreNetworkAttachmentId
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf segmentName
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
