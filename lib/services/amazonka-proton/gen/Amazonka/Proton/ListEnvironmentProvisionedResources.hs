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
-- Module      : Amazonka.Proton.ListEnvironmentProvisionedResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the provisioned resources for your environment.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListEnvironmentProvisionedResources
  ( -- * Creating a Request
    ListEnvironmentProvisionedResources (..),
    newListEnvironmentProvisionedResources,

    -- * Request Lenses
    listEnvironmentProvisionedResources_nextToken,
    listEnvironmentProvisionedResources_environmentName,

    -- * Destructuring the Response
    ListEnvironmentProvisionedResourcesResponse (..),
    newListEnvironmentProvisionedResourcesResponse,

    -- * Response Lenses
    listEnvironmentProvisionedResourcesResponse_nextToken,
    listEnvironmentProvisionedResourcesResponse_httpStatus,
    listEnvironmentProvisionedResourcesResponse_provisionedResources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironmentProvisionedResources' smart constructor.
data ListEnvironmentProvisionedResources = ListEnvironmentProvisionedResources'
  { -- | A token that indicates the location of the next environment provisioned
    -- resource in the array of environment provisioned resources, after the
    -- list of environment provisioned resources that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The environment name.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentProvisionedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentProvisionedResources_nextToken' - A token that indicates the location of the next environment provisioned
-- resource in the array of environment provisioned resources, after the
-- list of environment provisioned resources that was previously requested.
--
-- 'environmentName', 'listEnvironmentProvisionedResources_environmentName' - The environment name.
newListEnvironmentProvisionedResources ::
  -- | 'environmentName'
  Prelude.Text ->
  ListEnvironmentProvisionedResources
newListEnvironmentProvisionedResources
  pEnvironmentName_ =
    ListEnvironmentProvisionedResources'
      { nextToken =
          Prelude.Nothing,
        environmentName = pEnvironmentName_
      }

-- | A token that indicates the location of the next environment provisioned
-- resource in the array of environment provisioned resources, after the
-- list of environment provisioned resources that was previously requested.
listEnvironmentProvisionedResources_nextToken :: Lens.Lens' ListEnvironmentProvisionedResources (Prelude.Maybe Prelude.Text)
listEnvironmentProvisionedResources_nextToken = Lens.lens (\ListEnvironmentProvisionedResources' {nextToken} -> nextToken) (\s@ListEnvironmentProvisionedResources' {} a -> s {nextToken = a} :: ListEnvironmentProvisionedResources)

-- | The environment name.
listEnvironmentProvisionedResources_environmentName :: Lens.Lens' ListEnvironmentProvisionedResources Prelude.Text
listEnvironmentProvisionedResources_environmentName = Lens.lens (\ListEnvironmentProvisionedResources' {environmentName} -> environmentName) (\s@ListEnvironmentProvisionedResources' {} a -> s {environmentName = a} :: ListEnvironmentProvisionedResources)

instance
  Core.AWSPager
    ListEnvironmentProvisionedResources
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentProvisionedResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEnvironmentProvisionedResourcesResponse_provisionedResources
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEnvironmentProvisionedResources_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentProvisionedResourcesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListEnvironmentProvisionedResources
  where
  type
    AWSResponse ListEnvironmentProvisionedResources =
      ListEnvironmentProvisionedResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentProvisionedResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "provisionedResources"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListEnvironmentProvisionedResources
  where
  hashWithSalt
    _salt
    ListEnvironmentProvisionedResources' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` environmentName

instance
  Prelude.NFData
    ListEnvironmentProvisionedResources
  where
  rnf ListEnvironmentProvisionedResources' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentName

instance
  Data.ToHeaders
    ListEnvironmentProvisionedResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListEnvironmentProvisionedResources" ::
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
    ListEnvironmentProvisionedResources
  where
  toJSON ListEnvironmentProvisionedResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("environmentName" Data..= environmentName)
          ]
      )

instance
  Data.ToPath
    ListEnvironmentProvisionedResources
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListEnvironmentProvisionedResources
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnvironmentProvisionedResourcesResponse' smart constructor.
data ListEnvironmentProvisionedResourcesResponse = ListEnvironmentProvisionedResourcesResponse'
  { -- | A token that indicates the location of the next environment provisioned
    -- resource in the array of provisioned resources, after the current
    -- requested list of environment provisioned resources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of environment provisioned resources.
    provisionedResources :: [ProvisionedResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentProvisionedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentProvisionedResourcesResponse_nextToken' - A token that indicates the location of the next environment provisioned
-- resource in the array of provisioned resources, after the current
-- requested list of environment provisioned resources.
--
-- 'httpStatus', 'listEnvironmentProvisionedResourcesResponse_httpStatus' - The response's http status code.
--
-- 'provisionedResources', 'listEnvironmentProvisionedResourcesResponse_provisionedResources' - An array of environment provisioned resources.
newListEnvironmentProvisionedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentProvisionedResourcesResponse
newListEnvironmentProvisionedResourcesResponse
  pHttpStatus_ =
    ListEnvironmentProvisionedResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        provisionedResources =
          Prelude.mempty
      }

-- | A token that indicates the location of the next environment provisioned
-- resource in the array of provisioned resources, after the current
-- requested list of environment provisioned resources.
listEnvironmentProvisionedResourcesResponse_nextToken :: Lens.Lens' ListEnvironmentProvisionedResourcesResponse (Prelude.Maybe Prelude.Text)
listEnvironmentProvisionedResourcesResponse_nextToken = Lens.lens (\ListEnvironmentProvisionedResourcesResponse' {nextToken} -> nextToken) (\s@ListEnvironmentProvisionedResourcesResponse' {} a -> s {nextToken = a} :: ListEnvironmentProvisionedResourcesResponse)

-- | The response's http status code.
listEnvironmentProvisionedResourcesResponse_httpStatus :: Lens.Lens' ListEnvironmentProvisionedResourcesResponse Prelude.Int
listEnvironmentProvisionedResourcesResponse_httpStatus = Lens.lens (\ListEnvironmentProvisionedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentProvisionedResourcesResponse' {} a -> s {httpStatus = a} :: ListEnvironmentProvisionedResourcesResponse)

-- | An array of environment provisioned resources.
listEnvironmentProvisionedResourcesResponse_provisionedResources :: Lens.Lens' ListEnvironmentProvisionedResourcesResponse [ProvisionedResource]
listEnvironmentProvisionedResourcesResponse_provisionedResources = Lens.lens (\ListEnvironmentProvisionedResourcesResponse' {provisionedResources} -> provisionedResources) (\s@ListEnvironmentProvisionedResourcesResponse' {} a -> s {provisionedResources = a} :: ListEnvironmentProvisionedResourcesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEnvironmentProvisionedResourcesResponse
  where
  rnf ListEnvironmentProvisionedResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf provisionedResources
