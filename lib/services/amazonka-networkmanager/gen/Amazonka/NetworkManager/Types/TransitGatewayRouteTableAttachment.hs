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
-- Module      : Amazonka.NetworkManager.Types.TransitGatewayRouteTableAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.TransitGatewayRouteTableAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Attachment
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway route table attachment.
--
-- /See:/ 'newTransitGatewayRouteTableAttachment' smart constructor.
data TransitGatewayRouteTableAttachment = TransitGatewayRouteTableAttachment'
  { attachment :: Prelude.Maybe Attachment,
    -- | The ID of the peering attachment.
    peeringId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the transit gateway attachment route table.
    transitGatewayRouteTableArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRouteTableAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'transitGatewayRouteTableAttachment_attachment' - Undocumented member.
--
-- 'peeringId', 'transitGatewayRouteTableAttachment_peeringId' - The ID of the peering attachment.
--
-- 'transitGatewayRouteTableArn', 'transitGatewayRouteTableAttachment_transitGatewayRouteTableArn' - The ARN of the transit gateway attachment route table.
newTransitGatewayRouteTableAttachment ::
  TransitGatewayRouteTableAttachment
newTransitGatewayRouteTableAttachment =
  TransitGatewayRouteTableAttachment'
    { attachment =
        Prelude.Nothing,
      peeringId = Prelude.Nothing,
      transitGatewayRouteTableArn =
        Prelude.Nothing
    }

-- | Undocumented member.
transitGatewayRouteTableAttachment_attachment :: Lens.Lens' TransitGatewayRouteTableAttachment (Prelude.Maybe Attachment)
transitGatewayRouteTableAttachment_attachment = Lens.lens (\TransitGatewayRouteTableAttachment' {attachment} -> attachment) (\s@TransitGatewayRouteTableAttachment' {} a -> s {attachment = a} :: TransitGatewayRouteTableAttachment)

-- | The ID of the peering attachment.
transitGatewayRouteTableAttachment_peeringId :: Lens.Lens' TransitGatewayRouteTableAttachment (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAttachment_peeringId = Lens.lens (\TransitGatewayRouteTableAttachment' {peeringId} -> peeringId) (\s@TransitGatewayRouteTableAttachment' {} a -> s {peeringId = a} :: TransitGatewayRouteTableAttachment)

-- | The ARN of the transit gateway attachment route table.
transitGatewayRouteTableAttachment_transitGatewayRouteTableArn :: Lens.Lens' TransitGatewayRouteTableAttachment (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAttachment_transitGatewayRouteTableArn = Lens.lens (\TransitGatewayRouteTableAttachment' {transitGatewayRouteTableArn} -> transitGatewayRouteTableArn) (\s@TransitGatewayRouteTableAttachment' {} a -> s {transitGatewayRouteTableArn = a} :: TransitGatewayRouteTableAttachment)

instance
  Data.FromJSON
    TransitGatewayRouteTableAttachment
  where
  parseJSON =
    Data.withObject
      "TransitGatewayRouteTableAttachment"
      ( \x ->
          TransitGatewayRouteTableAttachment'
            Prelude.<$> (x Data..:? "Attachment")
            Prelude.<*> (x Data..:? "PeeringId")
            Prelude.<*> (x Data..:? "TransitGatewayRouteTableArn")
      )

instance
  Prelude.Hashable
    TransitGatewayRouteTableAttachment
  where
  hashWithSalt
    _salt
    TransitGatewayRouteTableAttachment' {..} =
      _salt
        `Prelude.hashWithSalt` attachment
        `Prelude.hashWithSalt` peeringId
        `Prelude.hashWithSalt` transitGatewayRouteTableArn

instance
  Prelude.NFData
    TransitGatewayRouteTableAttachment
  where
  rnf TransitGatewayRouteTableAttachment' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf peeringId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableArn
