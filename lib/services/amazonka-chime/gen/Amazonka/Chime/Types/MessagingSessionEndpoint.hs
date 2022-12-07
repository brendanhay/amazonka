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
-- Module      : Amazonka.Chime.Types.MessagingSessionEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.MessagingSessionEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON MessagingSessionEndpoint where
  parseJSON =
    Data.withObject
      "MessagingSessionEndpoint"
      ( \x ->
          MessagingSessionEndpoint'
            Prelude.<$> (x Data..:? "Url")
      )

instance Prelude.Hashable MessagingSessionEndpoint where
  hashWithSalt _salt MessagingSessionEndpoint' {..} =
    _salt `Prelude.hashWithSalt` url

instance Prelude.NFData MessagingSessionEndpoint where
  rnf MessagingSessionEndpoint' {..} = Prelude.rnf url
