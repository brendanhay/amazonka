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
-- Module      : Amazonka.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a service software update for an Amazon ES domain.
module Amazonka.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the
-- @StartElasticsearchServiceSoftwareUpdate@ operation. Specifies the name
-- of the Elasticsearch domain that you wish to schedule a service software
-- update on.
--
-- /See:/ 'newStartElasticsearchServiceSoftwareUpdate' smart constructor.
data StartElasticsearchServiceSoftwareUpdate = StartElasticsearchServiceSoftwareUpdate'
  { -- | The name of the domain that you want to update to the latest service
    -- software.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StartElasticsearchServiceSoftwareUpdate
newStartElasticsearchServiceSoftwareUpdate
  pDomainName_ =
    StartElasticsearchServiceSoftwareUpdate'
      { domainName =
          pDomainName_
      }

-- | The name of the domain that you want to update to the latest service
-- software.
startElasticsearchServiceSoftwareUpdate_domainName :: Lens.Lens' StartElasticsearchServiceSoftwareUpdate Prelude.Text
startElasticsearchServiceSoftwareUpdate_domainName = Lens.lens (\StartElasticsearchServiceSoftwareUpdate' {domainName} -> domainName) (\s@StartElasticsearchServiceSoftwareUpdate' {} a -> s {domainName = a} :: StartElasticsearchServiceSoftwareUpdate)

instance
  Core.AWSRequest
    StartElasticsearchServiceSoftwareUpdate
  where
  type
    AWSResponse
      StartElasticsearchServiceSoftwareUpdate =
      StartElasticsearchServiceSoftwareUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartElasticsearchServiceSoftwareUpdateResponse'
            Prelude.<$> (x Data..?> "ServiceSoftwareOptions")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartElasticsearchServiceSoftwareUpdate
  where
  hashWithSalt
    _salt
    StartElasticsearchServiceSoftwareUpdate' {..} =
      _salt `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    StartElasticsearchServiceSoftwareUpdate
  where
  rnf StartElasticsearchServiceSoftwareUpdate' {..} =
    Prelude.rnf domainName

instance
  Data.ToHeaders
    StartElasticsearchServiceSoftwareUpdate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    StartElasticsearchServiceSoftwareUpdate
  where
  toJSON StartElasticsearchServiceSoftwareUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance
  Data.ToPath
    StartElasticsearchServiceSoftwareUpdate
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/serviceSoftwareUpdate/start"

instance
  Data.ToQuery
    StartElasticsearchServiceSoftwareUpdate
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @StartElasticsearchServiceSoftwareUpdate@ operation.
-- Contains the status of the update.
--
-- /See:/ 'newStartElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data StartElasticsearchServiceSoftwareUpdateResponse = StartElasticsearchServiceSoftwareUpdateResponse'
  { -- | The current status of the Elasticsearch service software update.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartElasticsearchServiceSoftwareUpdateResponse
newStartElasticsearchServiceSoftwareUpdateResponse
  pHttpStatus_ =
    StartElasticsearchServiceSoftwareUpdateResponse'
      { serviceSoftwareOptions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current status of the Elasticsearch service software update.
startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse (Prelude.Maybe ServiceSoftwareOptions)
startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions = Lens.lens (\StartElasticsearchServiceSoftwareUpdateResponse' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@StartElasticsearchServiceSoftwareUpdateResponse' {} a -> s {serviceSoftwareOptions = a} :: StartElasticsearchServiceSoftwareUpdateResponse)

-- | The response's http status code.
startElasticsearchServiceSoftwareUpdateResponse_httpStatus :: Lens.Lens' StartElasticsearchServiceSoftwareUpdateResponse Prelude.Int
startElasticsearchServiceSoftwareUpdateResponse_httpStatus = Lens.lens (\StartElasticsearchServiceSoftwareUpdateResponse' {httpStatus} -> httpStatus) (\s@StartElasticsearchServiceSoftwareUpdateResponse' {} a -> s {httpStatus = a} :: StartElasticsearchServiceSoftwareUpdateResponse)

instance
  Prelude.NFData
    StartElasticsearchServiceSoftwareUpdateResponse
  where
  rnf
    StartElasticsearchServiceSoftwareUpdateResponse' {..} =
      Prelude.rnf serviceSoftwareOptions
        `Prelude.seq` Prelude.rnf httpStatus
