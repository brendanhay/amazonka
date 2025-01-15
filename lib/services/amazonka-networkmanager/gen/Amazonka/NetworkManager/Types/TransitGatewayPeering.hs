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
-- Module      : Amazonka.NetworkManager.Types.TransitGatewayPeering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.TransitGatewayPeering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Peering
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway peering attachment.
--
-- /See:/ 'newTransitGatewayPeering' smart constructor.
data TransitGatewayPeering = TransitGatewayPeering'
  { -- | Describes a transit gateway peer connection.
    peering :: Prelude.Maybe Peering,
    -- | The ARN of the transit gateway.
    transitGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway peering attachment.
    transitGatewayPeeringAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPeering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peering', 'transitGatewayPeering_peering' - Describes a transit gateway peer connection.
--
-- 'transitGatewayArn', 'transitGatewayPeering_transitGatewayArn' - The ARN of the transit gateway.
--
-- 'transitGatewayPeeringAttachmentId', 'transitGatewayPeering_transitGatewayPeeringAttachmentId' - The ID of the transit gateway peering attachment.
newTransitGatewayPeering ::
  TransitGatewayPeering
newTransitGatewayPeering =
  TransitGatewayPeering'
    { peering = Prelude.Nothing,
      transitGatewayArn = Prelude.Nothing,
      transitGatewayPeeringAttachmentId = Prelude.Nothing
    }

-- | Describes a transit gateway peer connection.
transitGatewayPeering_peering :: Lens.Lens' TransitGatewayPeering (Prelude.Maybe Peering)
transitGatewayPeering_peering = Lens.lens (\TransitGatewayPeering' {peering} -> peering) (\s@TransitGatewayPeering' {} a -> s {peering = a} :: TransitGatewayPeering)

-- | The ARN of the transit gateway.
transitGatewayPeering_transitGatewayArn :: Lens.Lens' TransitGatewayPeering (Prelude.Maybe Prelude.Text)
transitGatewayPeering_transitGatewayArn = Lens.lens (\TransitGatewayPeering' {transitGatewayArn} -> transitGatewayArn) (\s@TransitGatewayPeering' {} a -> s {transitGatewayArn = a} :: TransitGatewayPeering)

-- | The ID of the transit gateway peering attachment.
transitGatewayPeering_transitGatewayPeeringAttachmentId :: Lens.Lens' TransitGatewayPeering (Prelude.Maybe Prelude.Text)
transitGatewayPeering_transitGatewayPeeringAttachmentId = Lens.lens (\TransitGatewayPeering' {transitGatewayPeeringAttachmentId} -> transitGatewayPeeringAttachmentId) (\s@TransitGatewayPeering' {} a -> s {transitGatewayPeeringAttachmentId = a} :: TransitGatewayPeering)

instance Data.FromJSON TransitGatewayPeering where
  parseJSON =
    Data.withObject
      "TransitGatewayPeering"
      ( \x ->
          TransitGatewayPeering'
            Prelude.<$> (x Data..:? "Peering")
            Prelude.<*> (x Data..:? "TransitGatewayArn")
            Prelude.<*> (x Data..:? "TransitGatewayPeeringAttachmentId")
      )

instance Prelude.Hashable TransitGatewayPeering where
  hashWithSalt _salt TransitGatewayPeering' {..} =
    _salt
      `Prelude.hashWithSalt` peering
      `Prelude.hashWithSalt` transitGatewayArn
      `Prelude.hashWithSalt` transitGatewayPeeringAttachmentId

instance Prelude.NFData TransitGatewayPeering where
  rnf TransitGatewayPeering' {..} =
    Prelude.rnf peering `Prelude.seq`
      Prelude.rnf transitGatewayArn `Prelude.seq`
        Prelude.rnf transitGatewayPeeringAttachmentId
