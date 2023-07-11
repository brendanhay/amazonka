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
-- Module      : Amazonka.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a scheduled service software update for an Amazon ES domain. You
-- can only perform this operation before the @AutomatedUpdateDate@ and
-- when the @UpdateStatus@ is in the @PENDING_UPDATE@ state.
module Amazonka.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
  ( -- * Creating a Request
    CancelElasticsearchServiceSoftwareUpdate (..),
    newCancelElasticsearchServiceSoftwareUpdate,

    -- * Request Lenses
    cancelElasticsearchServiceSoftwareUpdate_domainName,

    -- * Destructuring the Response
    CancelElasticsearchServiceSoftwareUpdateResponse (..),
    newCancelElasticsearchServiceSoftwareUpdateResponse,

    -- * Response Lenses
    cancelElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    cancelElasticsearchServiceSoftwareUpdateResponse_httpStatus,
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
-- @CancelElasticsearchServiceSoftwareUpdate@ operation. Specifies the name
-- of the Elasticsearch domain that you wish to cancel a service software
-- update on.
--
-- /See:/ 'newCancelElasticsearchServiceSoftwareUpdate' smart constructor.
data CancelElasticsearchServiceSoftwareUpdate = CancelElasticsearchServiceSoftwareUpdate'
  { -- | The name of the domain that you want to stop the latest service software
    -- update on.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelElasticsearchServiceSoftwareUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'cancelElasticsearchServiceSoftwareUpdate_domainName' - The name of the domain that you want to stop the latest service software
-- update on.
newCancelElasticsearchServiceSoftwareUpdate ::
  -- | 'domainName'
  Prelude.Text ->
  CancelElasticsearchServiceSoftwareUpdate
newCancelElasticsearchServiceSoftwareUpdate
  pDomainName_ =
    CancelElasticsearchServiceSoftwareUpdate'
      { domainName =
          pDomainName_
      }

-- | The name of the domain that you want to stop the latest service software
-- update on.
cancelElasticsearchServiceSoftwareUpdate_domainName :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdate Prelude.Text
cancelElasticsearchServiceSoftwareUpdate_domainName = Lens.lens (\CancelElasticsearchServiceSoftwareUpdate' {domainName} -> domainName) (\s@CancelElasticsearchServiceSoftwareUpdate' {} a -> s {domainName = a} :: CancelElasticsearchServiceSoftwareUpdate)

instance
  Core.AWSRequest
    CancelElasticsearchServiceSoftwareUpdate
  where
  type
    AWSResponse
      CancelElasticsearchServiceSoftwareUpdate =
      CancelElasticsearchServiceSoftwareUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelElasticsearchServiceSoftwareUpdateResponse'
            Prelude.<$> (x Data..?> "ServiceSoftwareOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelElasticsearchServiceSoftwareUpdate
  where
  hashWithSalt
    _salt
    CancelElasticsearchServiceSoftwareUpdate' {..} =
      _salt `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    CancelElasticsearchServiceSoftwareUpdate
  where
  rnf CancelElasticsearchServiceSoftwareUpdate' {..} =
    Prelude.rnf domainName

instance
  Data.ToHeaders
    CancelElasticsearchServiceSoftwareUpdate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    CancelElasticsearchServiceSoftwareUpdate
  where
  toJSON CancelElasticsearchServiceSoftwareUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance
  Data.ToPath
    CancelElasticsearchServiceSoftwareUpdate
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/serviceSoftwareUpdate/cancel"

instance
  Data.ToQuery
    CancelElasticsearchServiceSoftwareUpdate
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CancelElasticsearchServiceSoftwareUpdate@ operation.
-- Contains the status of the update.
--
-- /See:/ 'newCancelElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data CancelElasticsearchServiceSoftwareUpdateResponse = CancelElasticsearchServiceSoftwareUpdateResponse'
  { -- | The current status of the Elasticsearch service software update.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelElasticsearchServiceSoftwareUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSoftwareOptions', 'cancelElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions' - The current status of the Elasticsearch service software update.
--
-- 'httpStatus', 'cancelElasticsearchServiceSoftwareUpdateResponse_httpStatus' - The response's http status code.
newCancelElasticsearchServiceSoftwareUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelElasticsearchServiceSoftwareUpdateResponse
newCancelElasticsearchServiceSoftwareUpdateResponse
  pHttpStatus_ =
    CancelElasticsearchServiceSoftwareUpdateResponse'
      { serviceSoftwareOptions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current status of the Elasticsearch service software update.
cancelElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdateResponse (Prelude.Maybe ServiceSoftwareOptions)
cancelElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions = Lens.lens (\CancelElasticsearchServiceSoftwareUpdateResponse' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@CancelElasticsearchServiceSoftwareUpdateResponse' {} a -> s {serviceSoftwareOptions = a} :: CancelElasticsearchServiceSoftwareUpdateResponse)

-- | The response's http status code.
cancelElasticsearchServiceSoftwareUpdateResponse_httpStatus :: Lens.Lens' CancelElasticsearchServiceSoftwareUpdateResponse Prelude.Int
cancelElasticsearchServiceSoftwareUpdateResponse_httpStatus = Lens.lens (\CancelElasticsearchServiceSoftwareUpdateResponse' {httpStatus} -> httpStatus) (\s@CancelElasticsearchServiceSoftwareUpdateResponse' {} a -> s {httpStatus = a} :: CancelElasticsearchServiceSoftwareUpdateResponse)

instance
  Prelude.NFData
    CancelElasticsearchServiceSoftwareUpdateResponse
  where
  rnf
    CancelElasticsearchServiceSoftwareUpdateResponse' {..} =
      Prelude.rnf serviceSoftwareOptions
        `Prelude.seq` Prelude.rnf httpStatus
