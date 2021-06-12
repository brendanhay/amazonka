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
-- Module      : Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a service software update for an Amazon ES domain.
module Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
  ( -- * Creating a Request
    StartElasticsearchServiceSoftwareUpdate (..),
    newStartElasticsearchServiceSoftwareUpdate,

    -- * Request Lenses
    startElasticsearchServiceSoftwareUpdate_domainName,

    -- * Destructuring the Response
    StartElasticsearchServiceSoftwareUpdateResponse (..),
    newStartElasticsearchServiceSoftwareUpdateResponse,

    -- * Response Lenses
    startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startElasticsearchServiceSoftwareUpdateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the
-- @StartElasticsearchServiceSoftwareUpdate@ operation. Specifies the name
-- of the Elasticsearch domain that you wish to schedule a service software
-- update on.
--
-- /See:/ 'newStartElasticsearchServiceSoftwareUpdate' smart constructor.
data StartElasticsearchServiceSoftwareUpdate = StartElasticsearchServiceSoftwareUpdate'
  { -- | The name of the domain that you want to update to the latest service
    -- software.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartElasticsearchServiceSoftwareUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'startElasticsearchServiceSoftwareUpdate_domainName' - The name of the domain that you want to update to the latest service
-- software.
newStartElasticsearchServiceSoftwareUpdate ::
  -- | 'domainName'
  Core.Text ->
  StartElasticsearchServiceSoftwareUpdate
newStartElasticsearchServiceSoftwareUpdate
  pDomainName_ =
    StartElasticsearchServiceSoftwareUpdate'
      { domainName =
          pDomainName_
      }

-- | The name of the domain that you want to update to the latest service
-- software.
startElasticsearchServiceSoftwareUpdate_domainName :: Lens.Lens' StartElasticsearchServiceSoftwareUpdate Core.Text
startElasticsearchServiceSoftwareUpdate_domainName = Lens.lens (\StartElasticsearchServiceSoftwareUpdate' {domainName} -> domainName) (\s@StartElasticsearchServiceSoftwareUpdate' {} a -> s {domainName = a} :: StartElasticsearchServiceSoftwareUpdate)

instance
  Core.AWSRequest
    StartElasticsearchServiceSoftwareUpdate
  where
  type
    AWSResponse
      StartElasticsearchServiceSoftwareUpdate =
      StartElasticsearchServiceSoftwareUpdateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartElasticsearchServiceSoftwareUpdateResponse'
            Core.<$> (x Core..?> "ServiceSoftwareOptions")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    StartElasticsearchServiceSoftwareUpdate

instance
  Core.NFData
    StartElasticsearchServiceSoftwareUpdate

instance
  Core.ToHeaders
    StartElasticsearchServiceSoftwareUpdate
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToJSON
    StartElasticsearchServiceSoftwareUpdate
  where
  toJSON StartElasticsearchServiceSoftwareUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DomainName" Core..= domainName)]
      )

instance
  Core.ToPath
    StartElasticsearchServiceSoftwareUpdate
  where
  toPath =
    Core.const
      "/2015-01-01/es/serviceSoftwareUpdate/start"

instance
  Core.ToQuery
    StartElasticsearchServiceSoftwareUpdate
  where
  toQuery = Core.const Core.mempty

-- | The result of a @StartElasticsearchServiceSoftwareUpdate@ operation.
-- Contains the status of the update.
--
-- /See:/ 'newStartElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data StartElasticsearchServiceSoftwareUpdateResponse = StartElasticsearchServiceSoftwareUpdateResponse'
  { -- | The current status of the Elasticsearch service software update.
    serviceSoftwareOptions :: Core.Maybe ServiceSoftwareOptions,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartElasticsearchServiceSoftwareUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSoftwareOptions', 'startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions' - The current status of the Elasticsearch service software update.
--
-- 'httpStatus', 'startElasticsearchServiceSoftwareUpdateResponse_httpStatus' - The response's http status code.
newStartElasticsearchServiceSoftwareUpdateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartElasticsearchServiceSoftwareUpdateResponse
newStartElasticsearchServiceSoftwareUpdateResponse
  pHttpStatus_ =
    StartElasticsearchServiceSoftwareUpdateResponse'
      { serviceSoftwareOptions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current status of the Elasticsearch service software update.
startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse (Core.Maybe ServiceSoftwareOptions)
startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions = Lens.lens (\StartElasticsearchServiceSoftwareUpdateResponse' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@StartElasticsearchServiceSoftwareUpdateResponse' {} a -> s {serviceSoftwareOptions = a} :: StartElasticsearchServiceSoftwareUpdateResponse)

-- | The response's http status code.
startElasticsearchServiceSoftwareUpdateResponse_httpStatus :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse Core.Int
startElasticsearchServiceSoftwareUpdateResponse_httpStatus = Lens.lens (\StartElasticsearchServiceSoftwareUpdateResponse' {httpStatus} -> httpStatus) (\s@StartElasticsearchServiceSoftwareUpdateResponse' {} a -> s {httpStatus = a} :: StartElasticsearchServiceSoftwareUpdateResponse)

instance
  Core.NFData
    StartElasticsearchServiceSoftwareUpdateResponse
