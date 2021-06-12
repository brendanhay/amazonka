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
-- Module      : Network.AWS.APIGateway.GetStages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more Stage resources.
module Network.AWS.APIGateway.GetStages
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about one or more Stage
-- resources.
--
-- /See:/ 'newGetStages' smart constructor.
data GetStages = GetStages'
  { -- | The stages\' deployment identifiers.
    deploymentId :: Core.Maybe Core.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'restApiId', 'getStages_restApiId' - [Required] The string identifier of the associated RestApi.
newGetStages ::
  -- | 'restApiId'
  Core.Text ->
  GetStages
newGetStages pRestApiId_ =
  GetStages'
    { deploymentId = Core.Nothing,
      restApiId = pRestApiId_
    }

-- | The stages\' deployment identifiers.
getStages_deploymentId :: Lens.Lens' GetStages (Core.Maybe Core.Text)
getStages_deploymentId = Lens.lens (\GetStages' {deploymentId} -> deploymentId) (\s@GetStages' {} a -> s {deploymentId = a} :: GetStages)

-- | [Required] The string identifier of the associated RestApi.
getStages_restApiId :: Lens.Lens' GetStages Core.Text
getStages_restApiId = Lens.lens (\GetStages' {restApiId} -> restApiId) (\s@GetStages' {} a -> s {restApiId = a} :: GetStages)

instance Core.AWSRequest GetStages where
  type AWSResponse GetStages = GetStagesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStagesResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetStages

instance Core.NFData GetStages

instance Core.ToHeaders GetStages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetStages where
  toPath GetStages' {..} =
    Core.mconcat
      ["/restapis/", Core.toBS restApiId, "/stages"]

instance Core.ToQuery GetStages where
  toQuery GetStages' {..} =
    Core.mconcat ["deploymentId" Core.=: deploymentId]

-- | A list of Stage resources that are associated with the ApiKey resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/stages.html Deploying API in Stages>
--
-- /See:/ 'newGetStagesResponse' smart constructor.
data GetStagesResponse = GetStagesResponse'
  { -- | The current page of elements from this collection.
    item :: Core.Maybe [Stage],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetStagesResponse
newGetStagesResponse pHttpStatus_ =
  GetStagesResponse'
    { item = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getStagesResponse_item :: Lens.Lens' GetStagesResponse (Core.Maybe [Stage])
getStagesResponse_item = Lens.lens (\GetStagesResponse' {item} -> item) (\s@GetStagesResponse' {} a -> s {item = a} :: GetStagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getStagesResponse_httpStatus :: Lens.Lens' GetStagesResponse Core.Int
getStagesResponse_httpStatus = Lens.lens (\GetStagesResponse' {httpStatus} -> httpStatus) (\s@GetStagesResponse' {} a -> s {httpStatus = a} :: GetStagesResponse)

instance Core.NFData GetStagesResponse
