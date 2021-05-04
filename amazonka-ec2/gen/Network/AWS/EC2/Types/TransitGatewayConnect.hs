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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnect where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayConnectOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a transit gateway Connect attachment.
--
-- /See:/ 'newTransitGatewayConnect' smart constructor.
data TransitGatewayConnect = TransitGatewayConnect'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The Connect attachment options.
    options :: Prelude.Maybe TransitGatewayConnectOptions,
    -- | The state of the attachment.
    state :: Prelude.Maybe TransitGatewayAttachmentState,
    -- | The tags for the attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Connect attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment from which the Connect attachment was created.
    transportTransitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'transitGatewayConnect_creationTime' - The creation time.
--
-- 'options', 'transitGatewayConnect_options' - The Connect attachment options.
--
-- 'state', 'transitGatewayConnect_state' - The state of the attachment.
--
-- 'tags', 'transitGatewayConnect_tags' - The tags for the attachment.
--
-- 'transitGatewayAttachmentId', 'transitGatewayConnect_transitGatewayAttachmentId' - The ID of the Connect attachment.
--
-- 'transportTransitGatewayAttachmentId', 'transitGatewayConnect_transportTransitGatewayAttachmentId' - The ID of the attachment from which the Connect attachment was created.
--
-- 'transitGatewayId', 'transitGatewayConnect_transitGatewayId' - The ID of the transit gateway.
newTransitGatewayConnect ::
  TransitGatewayConnect
newTransitGatewayConnect =
  TransitGatewayConnect'
    { creationTime =
        Prelude.Nothing,
      options = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      transportTransitGatewayAttachmentId =
        Prelude.Nothing,
      transitGatewayId = Prelude.Nothing
    }

-- | The creation time.
transitGatewayConnect_creationTime :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.UTCTime)
transitGatewayConnect_creationTime = Lens.lens (\TransitGatewayConnect' {creationTime} -> creationTime) (\s@TransitGatewayConnect' {} a -> s {creationTime = a} :: TransitGatewayConnect) Prelude.. Lens.mapping Prelude._Time

-- | The Connect attachment options.
transitGatewayConnect_options :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe TransitGatewayConnectOptions)
transitGatewayConnect_options = Lens.lens (\TransitGatewayConnect' {options} -> options) (\s@TransitGatewayConnect' {} a -> s {options = a} :: TransitGatewayConnect)

-- | The state of the attachment.
transitGatewayConnect_state :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe TransitGatewayAttachmentState)
transitGatewayConnect_state = Lens.lens (\TransitGatewayConnect' {state} -> state) (\s@TransitGatewayConnect' {} a -> s {state = a} :: TransitGatewayConnect)

-- | The tags for the attachment.
transitGatewayConnect_tags :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe [Tag])
transitGatewayConnect_tags = Lens.lens (\TransitGatewayConnect' {tags} -> tags) (\s@TransitGatewayConnect' {} a -> s {tags = a} :: TransitGatewayConnect) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the Connect attachment.
transitGatewayConnect_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.Text)
transitGatewayConnect_transitGatewayAttachmentId = Lens.lens (\TransitGatewayConnect' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayConnect' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayConnect)

-- | The ID of the attachment from which the Connect attachment was created.
transitGatewayConnect_transportTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.Text)
transitGatewayConnect_transportTransitGatewayAttachmentId = Lens.lens (\TransitGatewayConnect' {transportTransitGatewayAttachmentId} -> transportTransitGatewayAttachmentId) (\s@TransitGatewayConnect' {} a -> s {transportTransitGatewayAttachmentId = a} :: TransitGatewayConnect)

-- | The ID of the transit gateway.
transitGatewayConnect_transitGatewayId :: Lens.Lens' TransitGatewayConnect (Prelude.Maybe Prelude.Text)
transitGatewayConnect_transitGatewayId = Lens.lens (\TransitGatewayConnect' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayConnect' {} a -> s {transitGatewayId = a} :: TransitGatewayConnect)

instance Prelude.FromXML TransitGatewayConnect where
  parseXML x =
    TransitGatewayConnect'
      Prelude.<$> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "options")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Prelude..@? "transportTransitGatewayAttachmentId")
      Prelude.<*> (x Prelude..@? "transitGatewayId")

instance Prelude.Hashable TransitGatewayConnect

instance Prelude.NFData TransitGatewayConnect
