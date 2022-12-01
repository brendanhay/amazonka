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
-- Module      : Amazonka.EC2.Types.TransitGatewayConnect
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayConnect where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayAttachmentState
import Amazonka.EC2.Types.TransitGatewayConnectOptions
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway Connect attachment.
--
-- /See:/ 'newTransitGatewayConnect' smart constructor.
data TransitGatewayConnect = TransitGatewayConnect'
  { -- | The tags for the attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The state of the attachment.
    state :: Prelude.Maybe TransitGatewayAttachmentState,
    -- | The ID of the Connect attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The Connect attachment options.
    options :: Prelude.Maybe TransitGatewayConnectOptions,
    -- | The creation time.
    creationTime :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the attachment from which the Connect attachment was created.
    transportTransitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'transitGatewayConnect_tags' - The tags for the attachment.
--
-- 'transitGatewayId', 'transitGatewayConnect_transitGatewayId' - The ID of the transit gateway.
--
-- 'state', 'transitGatewayConnect_state' - The state of the attachment.
--
-- 'transitGatewayAttachmentId', 'transitGatewayConnect_transitGatewayAttachmentId' - The ID of the Connect attachment.
--
-- 'options', 'transitGatewayConnect_options' - The Connect attachment options.
--
-- 'creationTime', 'transitGatewayConnect_creationTime' - The creation time.
--
-- 'transportTransitGatewayAttachmentId', 'transitGatewayConnect_transportTransitGatewayAttachmentId' - The ID of the attachment from which the Connect attachment was created.
newTransitGatewayConnect ::
  TransitGatewayConnect
newTransitGatewayConnect =
  TransitGatewayConnect'
    { tags = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      options = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      transportTransitGatewayAttachmentId =
        Prelude.Nothing
    }

-- | The tags for the attachment.
transitGatewayConnect_tags :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe [Tag])
transitGatewayConnect_tags = Lens.lens (\TransitGatewayConnect' {tags} -> tags) (\s@TransitGatewayConnect' {} a -> s {tags = a} :: TransitGatewayConnect) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway.
transitGatewayConnect_transitGatewayId :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.Text)
transitGatewayConnect_transitGatewayId = Lens.lens (\TransitGatewayConnect' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayConnect' {} a -> s {transitGatewayId = a} :: TransitGatewayConnect)

-- | The state of the attachment.
transitGatewayConnect_state :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe TransitGatewayAttachmentState)
transitGatewayConnect_state = Lens.lens (\TransitGatewayConnect' {state} -> state) (\s@TransitGatewayConnect' {} a -> s {state = a} :: TransitGatewayConnect)

-- | The ID of the Connect attachment.
transitGatewayConnect_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.Text)
transitGatewayConnect_transitGatewayAttachmentId = Lens.lens (\TransitGatewayConnect' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayConnect' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayConnect)

-- | The Connect attachment options.
transitGatewayConnect_options :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe TransitGatewayConnectOptions)
transitGatewayConnect_options = Lens.lens (\TransitGatewayConnect' {options} -> options) (\s@TransitGatewayConnect' {} a -> s {options = a} :: TransitGatewayConnect)

-- | The creation time.
transitGatewayConnect_creationTime :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.UTCTime)
transitGatewayConnect_creationTime = Lens.lens (\TransitGatewayConnect' {creationTime} -> creationTime) (\s@TransitGatewayConnect' {} a -> s {creationTime = a} :: TransitGatewayConnect) Prelude.. Lens.mapping Core._Time

-- | The ID of the attachment from which the Connect attachment was created.
transitGatewayConnect_transportTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.Text)
transitGatewayConnect_transportTransitGatewayAttachmentId = Lens.lens (\TransitGatewayConnect' {transportTransitGatewayAttachmentId} -> transportTransitGatewayAttachmentId) (\s@TransitGatewayConnect' {} a -> s {transportTransitGatewayAttachmentId = a} :: TransitGatewayConnect)

instance Core.FromXML TransitGatewayConnect where
  parseXML x =
    TransitGatewayConnect'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "transitGatewayId")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Core..@? "options")
      Prelude.<*> (x Core..@? "creationTime")
      Prelude.<*> (x Core..@? "transportTransitGatewayAttachmentId")

instance Prelude.Hashable TransitGatewayConnect where
  hashWithSalt _salt TransitGatewayConnect' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` transitGatewayAttachmentId
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` transportTransitGatewayAttachmentId

instance Prelude.NFData TransitGatewayConnect where
  rnf TransitGatewayConnect' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf transportTransitGatewayAttachmentId
