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
-- Module      : Amazonka.IoTWireless.ListPositionConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List position configurations for a given resource, such as positioning
-- solvers.
module Amazonka.IoTWireless.ListPositionConfigurations
  ( -- * Creating a Request
    ListPositionConfigurations (..),
    newListPositionConfigurations,

    -- * Request Lenses
    listPositionConfigurations_resourceType,
    listPositionConfigurations_nextToken,
    listPositionConfigurations_maxResults,

    -- * Destructuring the Response
    ListPositionConfigurationsResponse (..),
    newListPositionConfigurationsResponse,

    -- * Response Lenses
    listPositionConfigurationsResponse_nextToken,
    listPositionConfigurationsResponse_positionConfigurationList,
    listPositionConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPositionConfigurations' smart constructor.
data ListPositionConfigurations = ListPositionConfigurations'
  { -- | Resource type for which position configurations are listed.
    resourceType :: Prelude.Maybe PositionResourceType,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPositionConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listPositionConfigurations_resourceType' - Resource type for which position configurations are listed.
--
-- 'nextToken', 'listPositionConfigurations_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listPositionConfigurations_maxResults' - Undocumented member.
newListPositionConfigurations ::
  ListPositionConfigurations
newListPositionConfigurations =
  ListPositionConfigurations'
    { resourceType =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Resource type for which position configurations are listed.
listPositionConfigurations_resourceType :: Lens.Lens' ListPositionConfigurations (Prelude.Maybe PositionResourceType)
listPositionConfigurations_resourceType = Lens.lens (\ListPositionConfigurations' {resourceType} -> resourceType) (\s@ListPositionConfigurations' {} a -> s {resourceType = a} :: ListPositionConfigurations)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listPositionConfigurations_nextToken :: Lens.Lens' ListPositionConfigurations (Prelude.Maybe Prelude.Text)
listPositionConfigurations_nextToken = Lens.lens (\ListPositionConfigurations' {nextToken} -> nextToken) (\s@ListPositionConfigurations' {} a -> s {nextToken = a} :: ListPositionConfigurations)

-- | Undocumented member.
listPositionConfigurations_maxResults :: Lens.Lens' ListPositionConfigurations (Prelude.Maybe Prelude.Natural)
listPositionConfigurations_maxResults = Lens.lens (\ListPositionConfigurations' {maxResults} -> maxResults) (\s@ListPositionConfigurations' {} a -> s {maxResults = a} :: ListPositionConfigurations)

instance Core.AWSRequest ListPositionConfigurations where
  type
    AWSResponse ListPositionConfigurations =
      ListPositionConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPositionConfigurationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "PositionConfigurationList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPositionConfigurations where
  hashWithSalt _salt ListPositionConfigurations' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPositionConfigurations where
  rnf ListPositionConfigurations' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListPositionConfigurations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPositionConfigurations where
  toPath = Prelude.const "/position-configurations"

instance Data.ToQuery ListPositionConfigurations where
  toQuery ListPositionConfigurations' {..} =
    Prelude.mconcat
      [ "resourceType" Data.=: resourceType,
        "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListPositionConfigurationsResponse' smart constructor.
data ListPositionConfigurationsResponse = ListPositionConfigurationsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of position configurations.
    positionConfigurationList :: Prelude.Maybe [PositionConfigurationItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPositionConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPositionConfigurationsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'positionConfigurationList', 'listPositionConfigurationsResponse_positionConfigurationList' - A list of position configurations.
--
-- 'httpStatus', 'listPositionConfigurationsResponse_httpStatus' - The response's http status code.
newListPositionConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPositionConfigurationsResponse
newListPositionConfigurationsResponse pHttpStatus_ =
  ListPositionConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      positionConfigurationList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listPositionConfigurationsResponse_nextToken :: Lens.Lens' ListPositionConfigurationsResponse (Prelude.Maybe Prelude.Text)
listPositionConfigurationsResponse_nextToken = Lens.lens (\ListPositionConfigurationsResponse' {nextToken} -> nextToken) (\s@ListPositionConfigurationsResponse' {} a -> s {nextToken = a} :: ListPositionConfigurationsResponse)

-- | A list of position configurations.
listPositionConfigurationsResponse_positionConfigurationList :: Lens.Lens' ListPositionConfigurationsResponse (Prelude.Maybe [PositionConfigurationItem])
listPositionConfigurationsResponse_positionConfigurationList = Lens.lens (\ListPositionConfigurationsResponse' {positionConfigurationList} -> positionConfigurationList) (\s@ListPositionConfigurationsResponse' {} a -> s {positionConfigurationList = a} :: ListPositionConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPositionConfigurationsResponse_httpStatus :: Lens.Lens' ListPositionConfigurationsResponse Prelude.Int
listPositionConfigurationsResponse_httpStatus = Lens.lens (\ListPositionConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListPositionConfigurationsResponse' {} a -> s {httpStatus = a} :: ListPositionConfigurationsResponse)

instance
  Prelude.NFData
    ListPositionConfigurationsResponse
  where
  rnf ListPositionConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf positionConfigurationList
      `Prelude.seq` Prelude.rnf httpStatus
