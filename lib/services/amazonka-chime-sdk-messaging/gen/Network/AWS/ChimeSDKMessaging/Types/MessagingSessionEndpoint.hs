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
-- Module      : Network.AWS.ChimeSDKMessaging.Types.MessagingSessionEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ChimeSDKMessaging.Types.MessagingSessionEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The websocket endpoint used to connect to Amazon Chime SDK messaging.
--
-- /See:/ 'newMessagingSessionEndpoint' smart constructor.
data MessagingSessionEndpoint = MessagingSessionEndpoint'
  { -- | The endpoint to which you establish a websocket connection.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessagingSessionEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'messagingSessionEndpoint_url' - The endpoint to which you establish a websocket connection.
newMessagingSessionEndpoint ::
  MessagingSessionEndpoint
newMessagingSessionEndpoint =
  MessagingSessionEndpoint' {url = Prelude.Nothing}

-- | The endpoint to which you establish a websocket connection.
messagingSessionEndpoint_url :: Lens.Lens' MessagingSessionEndpoint (Prelude.Maybe Prelude.Text)
messagingSessionEndpoint_url = Lens.lens (\MessagingSessionEndpoint' {url} -> url) (\s@MessagingSessionEndpoint' {} a -> s {url = a} :: MessagingSessionEndpoint)

instance Core.FromJSON MessagingSessionEndpoint where
  parseJSON =
    Core.withObject
      "MessagingSessionEndpoint"
      ( \x ->
          MessagingSessionEndpoint'
            Prelude.<$> (x Core..:? "Url")
      )

instance Prelude.Hashable MessagingSessionEndpoint

instance Prelude.NFData MessagingSessionEndpoint
