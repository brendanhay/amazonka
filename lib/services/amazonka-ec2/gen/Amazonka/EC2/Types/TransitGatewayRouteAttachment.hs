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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes a route attachment.
--
-- /See:/ 'newTransitGatewayRouteAttachment' smart constructor.
data TransitGatewayRouteAttachment = TransitGatewayRouteAttachment'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRouteAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayRouteAttachment_resourceId' - The ID of the resource.
--
-- 'resourceType', 'transitGatewayRouteAttachment_resourceType' - The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'transitGatewayAttachmentId', 'transitGatewayRouteAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newTransitGatewayRouteAttachment ::
  TransitGatewayRouteAttachment
newTransitGatewayRouteAttachment =
  TransitGatewayRouteAttachment'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayRouteAttachment_resourceId :: Lens.Lens' TransitGatewayRouteAttachment (Prelude.Maybe Prelude.Text)
transitGatewayRouteAttachment_resourceId = Lens.lens (\TransitGatewayRouteAttachment' {resourceId} -> resourceId) (\s@TransitGatewayRouteAttachment' {} a -> s {resourceId = a} :: TransitGatewayRouteAttachment)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayRouteAttachment_resourceType :: Lens.Lens' TransitGatewayRouteAttachment (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayRouteAttachment_resourceType = Lens.lens (\TransitGatewayRouteAttachment' {resourceType} -> resourceType) (\s@TransitGatewayRouteAttachment' {} a -> s {resourceType = a} :: TransitGatewayRouteAttachment)

-- | The ID of the attachment.
transitGatewayRouteAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteAttachment (Prelude.Maybe Prelude.Text)
transitGatewayRouteAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayRouteAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayRouteAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayRouteAttachment)

instance Data.FromXML TransitGatewayRouteAttachment where
  parseXML x =
    TransitGatewayRouteAttachment'
      Prelude.<$> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")

instance
  Prelude.Hashable
    TransitGatewayRouteAttachment
  where
  hashWithSalt _salt TransitGatewayRouteAttachment' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` transitGatewayAttachmentId

instance Prelude.NFData TransitGatewayRouteAttachment where
  rnf TransitGatewayRouteAttachment' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
