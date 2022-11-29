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
-- Module      : Amazonka.CloudWatchEvents.Types.EndpointEventBus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.EndpointEventBus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The event buses the endpoint is associated with.
--
-- /See:/ 'newEndpointEventBus' smart constructor.
data EndpointEventBus = EndpointEventBus'
  { -- | The ARN of the event bus the endpoint is associated with.
    eventBusArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointEventBus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusArn', 'endpointEventBus_eventBusArn' - The ARN of the event bus the endpoint is associated with.
newEndpointEventBus ::
  -- | 'eventBusArn'
  Prelude.Text ->
  EndpointEventBus
newEndpointEventBus pEventBusArn_ =
  EndpointEventBus' {eventBusArn = pEventBusArn_}

-- | The ARN of the event bus the endpoint is associated with.
endpointEventBus_eventBusArn :: Lens.Lens' EndpointEventBus Prelude.Text
endpointEventBus_eventBusArn = Lens.lens (\EndpointEventBus' {eventBusArn} -> eventBusArn) (\s@EndpointEventBus' {} a -> s {eventBusArn = a} :: EndpointEventBus)

instance Core.FromJSON EndpointEventBus where
  parseJSON =
    Core.withObject
      "EndpointEventBus"
      ( \x ->
          EndpointEventBus'
            Prelude.<$> (x Core..: "EventBusArn")
      )

instance Prelude.Hashable EndpointEventBus where
  hashWithSalt _salt EndpointEventBus' {..} =
    _salt `Prelude.hashWithSalt` eventBusArn

instance Prelude.NFData EndpointEventBus where
  rnf EndpointEventBus' {..} = Prelude.rnf eventBusArn

instance Core.ToJSON EndpointEventBus where
  toJSON EndpointEventBus' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("EventBusArn" Core..= eventBusArn)]
      )
