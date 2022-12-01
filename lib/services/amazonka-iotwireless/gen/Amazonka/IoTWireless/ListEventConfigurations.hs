{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTWireless.ListEventConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List event configurations where at least one event topic has been
-- enabled.
module Amazonka.IoTWireless.ListEventConfigurations
  ( -- * Creating a Request
    ListEventConfigurations (..),
    newListEventConfigurations,

    -- * Request Lenses
    listEventConfigurations_nextToken,
    listEventConfigurations_maxResults,
    listEventConfigurations_resourceType,

    -- * Destructuring the Response
    ListEventConfigurationsResponse (..),
    newListEventConfigurationsResponse,

    -- * Response Lenses
    listEventConfigurationsResponse_nextToken,
    listEventConfigurationsResponse_eventConfigurationsList,
    listEventConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventConfigurations' smart constructor.
data ListEventConfigurations = ListEventConfigurations'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Resource type to filter event configurations.
    resourceType :: EventNotificationResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventConfigurations_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listEventConfigurations_maxResults' - Undocumented member.
--
-- 'resourceType', 'listEventConfigurations_resourceType' - Resource type to filter event configurations.
newListEventConfigurations ::
  -- | 'resourceType'
  EventNotificationResourceType ->
  ListEventConfigurations
newListEventConfigurations pResourceType_ =
  ListEventConfigurations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceType = pResourceType_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listEventConfigurations_nextToken :: Lens.Lens' ListEventConfigurations (Prelude.Maybe Prelude.Text)
listEventConfigurations_nextToken = Lens.lens (\ListEventConfigurations' {nextToken} -> nextToken) (\s@ListEventConfigurations' {} a -> s {nextToken = a} :: ListEventConfigurations)

-- | Undocumented member.
listEventConfigurations_maxResults :: Lens.Lens' ListEventConfigurations (Prelude.Maybe Prelude.Natural)
listEventConfigurations_maxResults = Lens.lens (\ListEventConfigurations' {maxResults} -> maxResults) (\s@ListEventConfigurations' {} a -> s {maxResults = a} :: ListEventConfigurations)

-- | Resource type to filter event configurations.
listEventConfigurations_resourceType :: Lens.Lens' ListEventConfigurations EventNotificationResourceType
listEventConfigurations_resourceType = Lens.lens (\ListEventConfigurations' {resourceType} -> resourceType) (\s@ListEventConfigurations' {} a -> s {resourceType = a} :: ListEventConfigurations)

instance Core.AWSRequest ListEventConfigurations where
  type
    AWSResponse ListEventConfigurations =
      ListEventConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventConfigurationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "EventConfigurationsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventConfigurations where
  hashWithSalt _salt ListEventConfigurations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListEventConfigurations where
  rnf ListEventConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resourceType

instance Core.ToHeaders ListEventConfigurations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListEventConfigurations where
  toPath = Prelude.const "/event-configurations"

instance Core.ToQuery ListEventConfigurations where
  toQuery ListEventConfigurations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "resourceType" Core.=: resourceType
      ]

-- | /See:/ 'newListEventConfigurationsResponse' smart constructor.
data ListEventConfigurationsResponse = ListEventConfigurationsResponse'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Event configurations of all events for a single resource.
    eventConfigurationsList :: Prelude.Maybe [EventConfigurationItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventConfigurationsResponse_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'eventConfigurationsList', 'listEventConfigurationsResponse_eventConfigurationsList' - Event configurations of all events for a single resource.
--
-- 'httpStatus', 'listEventConfigurationsResponse_httpStatus' - The response's http status code.
newListEventConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventConfigurationsResponse
newListEventConfigurationsResponse pHttpStatus_ =
  ListEventConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      eventConfigurationsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listEventConfigurationsResponse_nextToken :: Lens.Lens' ListEventConfigurationsResponse (Prelude.Maybe Prelude.Text)
listEventConfigurationsResponse_nextToken = Lens.lens (\ListEventConfigurationsResponse' {nextToken} -> nextToken) (\s@ListEventConfigurationsResponse' {} a -> s {nextToken = a} :: ListEventConfigurationsResponse)

-- | Event configurations of all events for a single resource.
listEventConfigurationsResponse_eventConfigurationsList :: Lens.Lens' ListEventConfigurationsResponse (Prelude.Maybe [EventConfigurationItem])
listEventConfigurationsResponse_eventConfigurationsList = Lens.lens (\ListEventConfigurationsResponse' {eventConfigurationsList} -> eventConfigurationsList) (\s@ListEventConfigurationsResponse' {} a -> s {eventConfigurationsList = a} :: ListEventConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEventConfigurationsResponse_httpStatus :: Lens.Lens' ListEventConfigurationsResponse Prelude.Int
listEventConfigurationsResponse_httpStatus = Lens.lens (\ListEventConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListEventConfigurationsResponse' {} a -> s {httpStatus = a} :: ListEventConfigurationsResponse)

instance
  Prelude.NFData
    ListEventConfigurationsResponse
  where
  rnf ListEventConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eventConfigurationsList
      `Prelude.seq` Prelude.rnf httpStatus
