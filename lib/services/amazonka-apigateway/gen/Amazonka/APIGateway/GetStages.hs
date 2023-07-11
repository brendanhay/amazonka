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
-- Module      : Amazonka.APIGateway.GetStages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more Stage resources.
module Amazonka.APIGateway.GetStages
  ( -- * Creating a Request
    GetStages (..),
    newGetStages,

    -- * Request Lenses
    getStages_deploymentId,
    getStages_restApiId,

    -- * Destructuring the Response
    GetStagesResponse (..),
    newGetStagesResponse,

    -- * Response Lenses
    getStagesResponse_item,
    getStagesResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to get information about one or more Stage
-- resources.
--
-- /See:/ 'newGetStages' smart constructor.
data GetStages = GetStages'
  { -- | The stages\' deployment identifiers.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'getStages_deploymentId' - The stages\' deployment identifiers.
--
-- 'restApiId', 'getStages_restApiId' - The string identifier of the associated RestApi.
newGetStages ::
  -- | 'restApiId'
  Prelude.Text ->
  GetStages
newGetStages pRestApiId_ =
  GetStages'
    { deploymentId = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The stages\' deployment identifiers.
getStages_deploymentId :: Lens.Lens' GetStages (Prelude.Maybe Prelude.Text)
getStages_deploymentId = Lens.lens (\GetStages' {deploymentId} -> deploymentId) (\s@GetStages' {} a -> s {deploymentId = a} :: GetStages)

-- | The string identifier of the associated RestApi.
getStages_restApiId :: Lens.Lens' GetStages Prelude.Text
getStages_restApiId = Lens.lens (\GetStages' {restApiId} -> restApiId) (\s@GetStages' {} a -> s {restApiId = a} :: GetStages)

instance Core.AWSRequest GetStages where
  type AWSResponse GetStages = GetStagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStagesResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStages where
  hashWithSalt _salt GetStages' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetStages where
  rnf GetStages' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders GetStages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetStages where
  toPath GetStages' {..} =
    Prelude.mconcat
      ["/restapis/", Data.toBS restApiId, "/stages"]

instance Data.ToQuery GetStages where
  toQuery GetStages' {..} =
    Prelude.mconcat
      ["deploymentId" Data.=: deploymentId]

-- | A list of Stage resources that are associated with the ApiKey resource.
--
-- /See:/ 'newGetStagesResponse' smart constructor.
data GetStagesResponse = GetStagesResponse'
  { -- | The current page of elements from this collection.
    item :: Prelude.Maybe [Stage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'item', 'getStagesResponse_item' - The current page of elements from this collection.
--
-- 'httpStatus', 'getStagesResponse_httpStatus' - The response's http status code.
newGetStagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStagesResponse
newGetStagesResponse pHttpStatus_ =
  GetStagesResponse'
    { item = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getStagesResponse_item :: Lens.Lens' GetStagesResponse (Prelude.Maybe [Stage])
getStagesResponse_item = Lens.lens (\GetStagesResponse' {item} -> item) (\s@GetStagesResponse' {} a -> s {item = a} :: GetStagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getStagesResponse_httpStatus :: Lens.Lens' GetStagesResponse Prelude.Int
getStagesResponse_httpStatus = Lens.lens (\GetStagesResponse' {httpStatus} -> httpStatus) (\s@GetStagesResponse' {} a -> s {httpStatus = a} :: GetStagesResponse)

instance Prelude.NFData GetStagesResponse where
  rnf GetStagesResponse' {..} =
    Prelude.rnf item
      `Prelude.seq` Prelude.rnf httpStatus
