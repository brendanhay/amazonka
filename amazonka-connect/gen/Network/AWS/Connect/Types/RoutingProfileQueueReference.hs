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
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueReference where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the channel and queue identifier for a routing profile.
--
-- /See:/ 'newRoutingProfileQueueReference' smart constructor.
data RoutingProfileQueueReference = RoutingProfileQueueReference'
  { -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for
    -- this routing profile.
    channel :: Channel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfileQueueReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueId', 'routingProfileQueueReference_queueId' - The identifier for the queue.
--
-- 'channel', 'routingProfileQueueReference_channel' - The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
newRoutingProfileQueueReference ::
  -- | 'queueId'
  Prelude.Text ->
  -- | 'channel'
  Channel ->
  RoutingProfileQueueReference
newRoutingProfileQueueReference pQueueId_ pChannel_ =
  RoutingProfileQueueReference'
    { queueId = pQueueId_,
      channel = pChannel_
    }

-- | The identifier for the queue.
routingProfileQueueReference_queueId :: Lens.Lens' RoutingProfileQueueReference Prelude.Text
routingProfileQueueReference_queueId = Lens.lens (\RoutingProfileQueueReference' {queueId} -> queueId) (\s@RoutingProfileQueueReference' {} a -> s {queueId = a} :: RoutingProfileQueueReference)

-- | The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
routingProfileQueueReference_channel :: Lens.Lens' RoutingProfileQueueReference Channel
routingProfileQueueReference_channel = Lens.lens (\RoutingProfileQueueReference' {channel} -> channel) (\s@RoutingProfileQueueReference' {} a -> s {channel = a} :: RoutingProfileQueueReference)

instance
  Prelude.Hashable
    RoutingProfileQueueReference

instance Prelude.NFData RoutingProfileQueueReference

instance Prelude.ToJSON RoutingProfileQueueReference where
  toJSON RoutingProfileQueueReference' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("QueueId" Prelude..= queueId),
            Prelude.Just ("Channel" Prelude..= channel)
          ]
      )
