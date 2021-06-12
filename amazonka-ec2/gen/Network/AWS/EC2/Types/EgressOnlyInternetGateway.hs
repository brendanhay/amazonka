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
-- Module      : Network.AWS.EC2.Types.EgressOnlyInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EgressOnlyInternetGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an egress-only internet gateway.
--
-- /See:/ 'newEgressOnlyInternetGateway' smart constructor.
data EgressOnlyInternetGateway = EgressOnlyInternetGateway'
  { -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Maybe Core.Text,
    -- | The tags assigned to the egress-only internet gateway.
    tags :: Core.Maybe [Tag],
    -- | Information about the attachment of the egress-only internet gateway.
    attachments :: Core.Maybe [InternetGatewayAttachment]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EgressOnlyInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressOnlyInternetGatewayId', 'egressOnlyInternetGateway_egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
--
-- 'tags', 'egressOnlyInternetGateway_tags' - The tags assigned to the egress-only internet gateway.
--
-- 'attachments', 'egressOnlyInternetGateway_attachments' - Information about the attachment of the egress-only internet gateway.
newEgressOnlyInternetGateway ::
  EgressOnlyInternetGateway
newEgressOnlyInternetGateway =
  EgressOnlyInternetGateway'
    { egressOnlyInternetGatewayId =
        Core.Nothing,
      tags = Core.Nothing,
      attachments = Core.Nothing
    }

-- | The ID of the egress-only internet gateway.
egressOnlyInternetGateway_egressOnlyInternetGatewayId :: Lens.Lens' EgressOnlyInternetGateway (Core.Maybe Core.Text)
egressOnlyInternetGateway_egressOnlyInternetGatewayId = Lens.lens (\EgressOnlyInternetGateway' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@EgressOnlyInternetGateway' {} a -> s {egressOnlyInternetGatewayId = a} :: EgressOnlyInternetGateway)

-- | The tags assigned to the egress-only internet gateway.
egressOnlyInternetGateway_tags :: Lens.Lens' EgressOnlyInternetGateway (Core.Maybe [Tag])
egressOnlyInternetGateway_tags = Lens.lens (\EgressOnlyInternetGateway' {tags} -> tags) (\s@EgressOnlyInternetGateway' {} a -> s {tags = a} :: EgressOnlyInternetGateway) Core.. Lens.mapping Lens._Coerce

-- | Information about the attachment of the egress-only internet gateway.
egressOnlyInternetGateway_attachments :: Lens.Lens' EgressOnlyInternetGateway (Core.Maybe [InternetGatewayAttachment])
egressOnlyInternetGateway_attachments = Lens.lens (\EgressOnlyInternetGateway' {attachments} -> attachments) (\s@EgressOnlyInternetGateway' {} a -> s {attachments = a} :: EgressOnlyInternetGateway) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML EgressOnlyInternetGateway where
  parseXML x =
    EgressOnlyInternetGateway'
      Core.<$> (x Core..@? "egressOnlyInternetGatewayId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "attachmentSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable EgressOnlyInternetGateway

instance Core.NFData EgressOnlyInternetGateway
