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
-- Module      : Amazonka.Connect.Types.RuleTriggerEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RuleTriggerEventSource where

import Amazonka.Connect.Types.EventSourceName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of the event source. This field is required if
-- @TriggerEventSource@ is one of the following values:
-- @OnZendeskTicketCreate@ | @OnZendeskTicketStatusUpdate@ |
-- @OnSalesforceCaseCreate@
--
-- /See:/ 'newRuleTriggerEventSource' smart constructor.
data RuleTriggerEventSource = RuleTriggerEventSource'
  { -- | The identifier for the integration association.
    integrationAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the event source.
    eventSourceName :: EventSourceName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleTriggerEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integrationAssociationId', 'ruleTriggerEventSource_integrationAssociationId' - The identifier for the integration association.
--
-- 'eventSourceName', 'ruleTriggerEventSource_eventSourceName' - The name of the event source.
newRuleTriggerEventSource ::
  -- | 'eventSourceName'
  EventSourceName ->
  RuleTriggerEventSource
newRuleTriggerEventSource pEventSourceName_ =
  RuleTriggerEventSource'
    { integrationAssociationId =
        Prelude.Nothing,
      eventSourceName = pEventSourceName_
    }

-- | The identifier for the integration association.
ruleTriggerEventSource_integrationAssociationId :: Lens.Lens' RuleTriggerEventSource (Prelude.Maybe Prelude.Text)
ruleTriggerEventSource_integrationAssociationId = Lens.lens (\RuleTriggerEventSource' {integrationAssociationId} -> integrationAssociationId) (\s@RuleTriggerEventSource' {} a -> s {integrationAssociationId = a} :: RuleTriggerEventSource)

-- | The name of the event source.
ruleTriggerEventSource_eventSourceName :: Lens.Lens' RuleTriggerEventSource EventSourceName
ruleTriggerEventSource_eventSourceName = Lens.lens (\RuleTriggerEventSource' {eventSourceName} -> eventSourceName) (\s@RuleTriggerEventSource' {} a -> s {eventSourceName = a} :: RuleTriggerEventSource)

instance Data.FromJSON RuleTriggerEventSource where
  parseJSON =
    Data.withObject
      "RuleTriggerEventSource"
      ( \x ->
          RuleTriggerEventSource'
            Prelude.<$> (x Data..:? "IntegrationAssociationId")
            Prelude.<*> (x Data..: "EventSourceName")
      )

instance Prelude.Hashable RuleTriggerEventSource where
  hashWithSalt _salt RuleTriggerEventSource' {..} =
    _salt
      `Prelude.hashWithSalt` integrationAssociationId
      `Prelude.hashWithSalt` eventSourceName

instance Prelude.NFData RuleTriggerEventSource where
  rnf RuleTriggerEventSource' {..} =
    Prelude.rnf integrationAssociationId
      `Prelude.seq` Prelude.rnf eventSourceName

instance Data.ToJSON RuleTriggerEventSource where
  toJSON RuleTriggerEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IntegrationAssociationId" Data..=)
              Prelude.<$> integrationAssociationId,
            Prelude.Just
              ("EventSourceName" Data..= eventSourceName)
          ]
      )
