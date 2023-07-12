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
-- Module      : Amazonka.Proton.ListComponents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List components with summary data. You can filter the result list by
-- environment, service, or a single service instance.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListComponents
  ( -- * Creating a Request
    ListComponents (..),
    newListComponents,

    -- * Request Lenses
    listComponents_environmentName,
    listComponents_maxResults,
    listComponents_nextToken,
    listComponents_serviceInstanceName,
    listComponents_serviceName,

    -- * Destructuring the Response
    ListComponentsResponse (..),
    newListComponentsResponse,

    -- * Response Lenses
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,
    listComponentsResponse_components,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponents' smart constructor.
data ListComponents = ListComponents'
  { -- | The name of an environment for result list filtering. Proton returns
    -- components associated with the environment or attached to service
    -- instances running in it.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of components to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the location of the next component in the array
    -- of components, after the list of components that was previously
    -- requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of a service instance for result list filtering. Proton returns
    -- the component attached to the service instance, if any.
    serviceInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The name of a service for result list filtering. Proton returns
    -- components attached to service instances of the service.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'listComponents_environmentName' - The name of an environment for result list filtering. Proton returns
-- components associated with the environment or attached to service
-- instances running in it.
--
-- 'maxResults', 'listComponents_maxResults' - The maximum number of components to list.
--
-- 'nextToken', 'listComponents_nextToken' - A token that indicates the location of the next component in the array
-- of components, after the list of components that was previously
-- requested.
--
-- 'serviceInstanceName', 'listComponents_serviceInstanceName' - The name of a service instance for result list filtering. Proton returns
-- the component attached to the service instance, if any.
--
-- 'serviceName', 'listComponents_serviceName' - The name of a service for result list filtering. Proton returns
-- components attached to service instances of the service.
newListComponents ::
  ListComponents
newListComponents =
  ListComponents'
    { environmentName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceInstanceName = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The name of an environment for result list filtering. Proton returns
-- components associated with the environment or attached to service
-- instances running in it.
listComponents_environmentName :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Text)
listComponents_environmentName = Lens.lens (\ListComponents' {environmentName} -> environmentName) (\s@ListComponents' {} a -> s {environmentName = a} :: ListComponents)

-- | The maximum number of components to list.
listComponents_maxResults :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Natural)
listComponents_maxResults = Lens.lens (\ListComponents' {maxResults} -> maxResults) (\s@ListComponents' {} a -> s {maxResults = a} :: ListComponents)

-- | A token that indicates the location of the next component in the array
-- of components, after the list of components that was previously
-- requested.
listComponents_nextToken :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Text)
listComponents_nextToken = Lens.lens (\ListComponents' {nextToken} -> nextToken) (\s@ListComponents' {} a -> s {nextToken = a} :: ListComponents)

-- | The name of a service instance for result list filtering. Proton returns
-- the component attached to the service instance, if any.
listComponents_serviceInstanceName :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Text)
listComponents_serviceInstanceName = Lens.lens (\ListComponents' {serviceInstanceName} -> serviceInstanceName) (\s@ListComponents' {} a -> s {serviceInstanceName = a} :: ListComponents)

-- | The name of a service for result list filtering. Proton returns
-- components attached to service instances of the service.
listComponents_serviceName :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Text)
listComponents_serviceName = Lens.lens (\ListComponents' {serviceName} -> serviceName) (\s@ListComponents' {} a -> s {serviceName = a} :: ListComponents)

instance Core.AWSPager ListComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComponentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listComponentsResponse_components) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listComponents_nextToken
          Lens..~ rs
          Lens.^? listComponentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListComponents where
  type
    AWSResponse ListComponents =
      ListComponentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComponentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "components" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListComponents where
  hashWithSalt _salt ListComponents' {..} =
    _salt
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceInstanceName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ListComponents where
  rnf ListComponents' {..} =
    Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders ListComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListComponents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListComponents where
  toJSON ListComponents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("environmentName" Data..=)
              Prelude.<$> environmentName,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("serviceInstanceName" Data..=)
              Prelude.<$> serviceInstanceName,
            ("serviceName" Data..=) Prelude.<$> serviceName
          ]
      )

instance Data.ToPath ListComponents where
  toPath = Prelude.const "/"

instance Data.ToQuery ListComponents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComponentsResponse' smart constructor.
data ListComponentsResponse = ListComponentsResponse'
  { -- | A token that indicates the location of the next component in the array
    -- of components, after the current requested list of components.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of components with summary data.
    components :: [ComponentSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentsResponse_nextToken' - A token that indicates the location of the next component in the array
-- of components, after the current requested list of components.
--
-- 'httpStatus', 'listComponentsResponse_httpStatus' - The response's http status code.
--
-- 'components', 'listComponentsResponse_components' - An array of components with summary data.
newListComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComponentsResponse
newListComponentsResponse pHttpStatus_ =
  ListComponentsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      components = Prelude.mempty
    }

-- | A token that indicates the location of the next component in the array
-- of components, after the current requested list of components.
listComponentsResponse_nextToken :: Lens.Lens' ListComponentsResponse (Prelude.Maybe Prelude.Text)
listComponentsResponse_nextToken = Lens.lens (\ListComponentsResponse' {nextToken} -> nextToken) (\s@ListComponentsResponse' {} a -> s {nextToken = a} :: ListComponentsResponse)

-- | The response's http status code.
listComponentsResponse_httpStatus :: Lens.Lens' ListComponentsResponse Prelude.Int
listComponentsResponse_httpStatus = Lens.lens (\ListComponentsResponse' {httpStatus} -> httpStatus) (\s@ListComponentsResponse' {} a -> s {httpStatus = a} :: ListComponentsResponse)

-- | An array of components with summary data.
listComponentsResponse_components :: Lens.Lens' ListComponentsResponse [ComponentSummary]
listComponentsResponse_components = Lens.lens (\ListComponentsResponse' {components} -> components) (\s@ListComponentsResponse' {} a -> s {components = a} :: ListComponentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListComponentsResponse where
  rnf ListComponentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf components
