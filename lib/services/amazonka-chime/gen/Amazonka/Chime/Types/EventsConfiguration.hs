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
-- Module      : Amazonka.Chime.Types.EventsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.EventsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration that allows a bot to receive outgoing events. Can be
-- either an HTTPS endpoint or a Lambda function ARN.
--
-- /See:/ 'newEventsConfiguration' smart constructor.
data EventsConfiguration = EventsConfiguration'
  { -- | HTTPS endpoint that allows a bot to receive outgoing events.
    outboundEventsHTTPSEndpoint :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The bot ID.
    botId :: Prelude.Maybe Prelude.Text,
    -- | Lambda function ARN that allows a bot to receive outgoing events.
    lambdaFunctionArn :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outboundEventsHTTPSEndpoint', 'eventsConfiguration_outboundEventsHTTPSEndpoint' - HTTPS endpoint that allows a bot to receive outgoing events.
--
-- 'botId', 'eventsConfiguration_botId' - The bot ID.
--
-- 'lambdaFunctionArn', 'eventsConfiguration_lambdaFunctionArn' - Lambda function ARN that allows a bot to receive outgoing events.
newEventsConfiguration ::
  EventsConfiguration
newEventsConfiguration =
  EventsConfiguration'
    { outboundEventsHTTPSEndpoint =
        Prelude.Nothing,
      botId = Prelude.Nothing,
      lambdaFunctionArn = Prelude.Nothing
    }

-- | HTTPS endpoint that allows a bot to receive outgoing events.
eventsConfiguration_outboundEventsHTTPSEndpoint :: Lens.Lens' EventsConfiguration (Prelude.Maybe Prelude.Text)
eventsConfiguration_outboundEventsHTTPSEndpoint = Lens.lens (\EventsConfiguration' {outboundEventsHTTPSEndpoint} -> outboundEventsHTTPSEndpoint) (\s@EventsConfiguration' {} a -> s {outboundEventsHTTPSEndpoint = a} :: EventsConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The bot ID.
eventsConfiguration_botId :: Lens.Lens' EventsConfiguration (Prelude.Maybe Prelude.Text)
eventsConfiguration_botId = Lens.lens (\EventsConfiguration' {botId} -> botId) (\s@EventsConfiguration' {} a -> s {botId = a} :: EventsConfiguration)

-- | Lambda function ARN that allows a bot to receive outgoing events.
eventsConfiguration_lambdaFunctionArn :: Lens.Lens' EventsConfiguration (Prelude.Maybe Prelude.Text)
eventsConfiguration_lambdaFunctionArn = Lens.lens (\EventsConfiguration' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@EventsConfiguration' {} a -> s {lambdaFunctionArn = a} :: EventsConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON EventsConfiguration where
  parseJSON =
    Data.withObject
      "EventsConfiguration"
      ( \x ->
          EventsConfiguration'
            Prelude.<$> (x Data..:? "OutboundEventsHTTPSEndpoint")
            Prelude.<*> (x Data..:? "BotId")
            Prelude.<*> (x Data..:? "LambdaFunctionArn")
      )

instance Prelude.Hashable EventsConfiguration where
  hashWithSalt _salt EventsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` outboundEventsHTTPSEndpoint
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` lambdaFunctionArn

instance Prelude.NFData EventsConfiguration where
  rnf EventsConfiguration' {..} =
    Prelude.rnf outboundEventsHTTPSEndpoint
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf lambdaFunctionArn
