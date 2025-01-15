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
-- Module      : Amazonka.Proton.ListComponentProvisionedResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List provisioned resources for a component with details.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListComponentProvisionedResources
  ( -- * Creating a Request
    ListComponentProvisionedResources (..),
    newListComponentProvisionedResources,

    -- * Request Lenses
    listComponentProvisionedResources_nextToken,
    listComponentProvisionedResources_componentName,

    -- * Destructuring the Response
    ListComponentProvisionedResourcesResponse (..),
    newListComponentProvisionedResourcesResponse,

    -- * Response Lenses
    listComponentProvisionedResourcesResponse_nextToken,
    listComponentProvisionedResourcesResponse_httpStatus,
    listComponentProvisionedResourcesResponse_provisionedResources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponentProvisionedResources' smart constructor.
data ListComponentProvisionedResources = ListComponentProvisionedResources'
  { -- | A token that indicates the location of the next provisioned resource in
    -- the array of provisioned resources, after the list of provisioned
    -- resources that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the component whose provisioned resources you want.
    componentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentProvisionedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentProvisionedResources_nextToken' - A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the list of provisioned
-- resources that was previously requested.
--
-- 'componentName', 'listComponentProvisionedResources_componentName' - The name of the component whose provisioned resources you want.
newListComponentProvisionedResources ::
  -- | 'componentName'
  Prelude.Text ->
  ListComponentProvisionedResources
newListComponentProvisionedResources pComponentName_ =
  ListComponentProvisionedResources'
    { nextToken =
        Prelude.Nothing,
      componentName = pComponentName_
    }

-- | A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the list of provisioned
-- resources that was previously requested.
listComponentProvisionedResources_nextToken :: Lens.Lens' ListComponentProvisionedResources (Prelude.Maybe Prelude.Text)
listComponentProvisionedResources_nextToken = Lens.lens (\ListComponentProvisionedResources' {nextToken} -> nextToken) (\s@ListComponentProvisionedResources' {} a -> s {nextToken = a} :: ListComponentProvisionedResources)

-- | The name of the component whose provisioned resources you want.
listComponentProvisionedResources_componentName :: Lens.Lens' ListComponentProvisionedResources Prelude.Text
listComponentProvisionedResources_componentName = Lens.lens (\ListComponentProvisionedResources' {componentName} -> componentName) (\s@ListComponentProvisionedResources' {} a -> s {componentName = a} :: ListComponentProvisionedResources)

instance
  Core.AWSPager
    ListComponentProvisionedResources
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComponentProvisionedResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listComponentProvisionedResourcesResponse_provisionedResources
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listComponentProvisionedResources_nextToken
              Lens..~ rs
              Lens.^? listComponentProvisionedResourcesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListComponentProvisionedResources
  where
  type
    AWSResponse ListComponentProvisionedResources =
      ListComponentProvisionedResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComponentProvisionedResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "provisionedResources"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListComponentProvisionedResources
  where
  hashWithSalt
    _salt
    ListComponentProvisionedResources' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` componentName

instance
  Prelude.NFData
    ListComponentProvisionedResources
  where
  rnf ListComponentProvisionedResources' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf componentName

instance
  Data.ToHeaders
    ListComponentProvisionedResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListComponentProvisionedResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListComponentProvisionedResources
  where
  toJSON ListComponentProvisionedResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("componentName" Data..= componentName)
          ]
      )

instance
  Data.ToPath
    ListComponentProvisionedResources
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListComponentProvisionedResources
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComponentProvisionedResourcesResponse' smart constructor.
data ListComponentProvisionedResourcesResponse = ListComponentProvisionedResourcesResponse'
  { -- | A token that indicates the location of the next provisioned resource in
    -- the array of provisioned resources, after the current requested list of
    -- provisioned resources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of provisioned resources for a component.
    provisionedResources :: [ProvisionedResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentProvisionedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentProvisionedResourcesResponse_nextToken' - A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the current requested list of
-- provisioned resources.
--
-- 'httpStatus', 'listComponentProvisionedResourcesResponse_httpStatus' - The response's http status code.
--
-- 'provisionedResources', 'listComponentProvisionedResourcesResponse_provisionedResources' - An array of provisioned resources for a component.
newListComponentProvisionedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComponentProvisionedResourcesResponse
newListComponentProvisionedResourcesResponse
  pHttpStatus_ =
    ListComponentProvisionedResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        provisionedResources =
          Prelude.mempty
      }

-- | A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the current requested list of
-- provisioned resources.
listComponentProvisionedResourcesResponse_nextToken :: Lens.Lens' ListComponentProvisionedResourcesResponse (Prelude.Maybe Prelude.Text)
listComponentProvisionedResourcesResponse_nextToken = Lens.lens (\ListComponentProvisionedResourcesResponse' {nextToken} -> nextToken) (\s@ListComponentProvisionedResourcesResponse' {} a -> s {nextToken = a} :: ListComponentProvisionedResourcesResponse)

-- | The response's http status code.
listComponentProvisionedResourcesResponse_httpStatus :: Lens.Lens' ListComponentProvisionedResourcesResponse Prelude.Int
listComponentProvisionedResourcesResponse_httpStatus = Lens.lens (\ListComponentProvisionedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListComponentProvisionedResourcesResponse' {} a -> s {httpStatus = a} :: ListComponentProvisionedResourcesResponse)

-- | An array of provisioned resources for a component.
listComponentProvisionedResourcesResponse_provisionedResources :: Lens.Lens' ListComponentProvisionedResourcesResponse [ProvisionedResource]
listComponentProvisionedResourcesResponse_provisionedResources = Lens.lens (\ListComponentProvisionedResourcesResponse' {provisionedResources} -> provisionedResources) (\s@ListComponentProvisionedResourcesResponse' {} a -> s {provisionedResources = a} :: ListComponentProvisionedResourcesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListComponentProvisionedResourcesResponse
  where
  rnf ListComponentProvisionedResourcesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf provisionedResources
