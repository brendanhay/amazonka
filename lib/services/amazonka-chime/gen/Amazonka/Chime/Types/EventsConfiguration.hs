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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.EventsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration that allows a bot to receive outgoing events. Can be
-- either an HTTPS endpoint or a Lambda function ARN.
--
-- /See:/ 'newEventsConfiguration' smart constructor.
data EventsConfiguration = EventsConfiguration'
  { -- | Lambda function ARN that allows a bot to receive outgoing events.
    lambdaFunctionArn :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The bot ID.
    botId :: Prelude.Maybe Prelude.Text,
    -- | HTTPS endpoint that allows a bot to receive outgoing events.
    outboundEventsHTTPSEndpoint :: Prelude.Maybe (Core.Sensitive Prelude.Text)
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
-- 'lambdaFunctionArn', 'eventsConfiguration_lambdaFunctionArn' - Lambda function ARN that allows a bot to receive outgoing events.
--
-- 'botId', 'eventsConfiguration_botId' - The bot ID.
--
-- 'outboundEventsHTTPSEndpoint', 'eventsConfiguration_outboundEventsHTTPSEndpoint' - HTTPS endpoint that allows a bot to receive outgoing events.
newEventsConfiguration ::
  EventsConfiguration
newEventsConfiguration =
  EventsConfiguration'
    { lambdaFunctionArn =
        Prelude.Nothing,
      botId = Prelude.Nothing,
      outboundEventsHTTPSEndpoint = Prelude.Nothing
    }

-- | Lambda function ARN that allows a bot to receive outgoing events.
eventsConfiguration_lambdaFunctionArn :: Lens.Lens' EventsConfiguration (Prelude.Maybe Prelude.Text)
eventsConfiguration_lambdaFunctionArn = Lens.lens (\EventsConfiguration' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@EventsConfiguration' {} a -> s {lambdaFunctionArn = a} :: EventsConfiguration) Prelude.. Lens.mapping Core._Sensitive

-- | The bot ID.
eventsConfiguration_botId :: Lens.Lens' EventsConfiguration (Prelude.Maybe Prelude.Text)
eventsConfiguration_botId = Lens.lens (\EventsConfiguration' {botId} -> botId) (\s@EventsConfiguration' {} a -> s {botId = a} :: EventsConfiguration)

-- | HTTPS endpoint that allows a bot to receive outgoing events.
eventsConfiguration_outboundEventsHTTPSEndpoint :: Lens.Lens' EventsConfiguration (Prelude.Maybe Prelude.Text)
eventsConfiguration_outboundEventsHTTPSEndpoint = Lens.lens (\EventsConfiguration' {outboundEventsHTTPSEndpoint} -> outboundEventsHTTPSEndpoint) (\s@EventsConfiguration' {} a -> s {outboundEventsHTTPSEndpoint = a} :: EventsConfiguration) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON EventsConfiguration where
  parseJSON =
    Core.withObject
      "EventsConfiguration"
      ( \x ->
          EventsConfiguration'
            Prelude.<$> (x Core..:? "LambdaFunctionArn")
            Prelude.<*> (x Core..:? "BotId")
            Prelude.<*> (x Core..:? "OutboundEventsHTTPSEndpoint")
      )

instance Prelude.Hashable EventsConfiguration

instance Prelude.NFData EventsConfiguration
