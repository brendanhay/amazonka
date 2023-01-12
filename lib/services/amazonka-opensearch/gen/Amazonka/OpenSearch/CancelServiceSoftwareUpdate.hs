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
-- Module      : Amazonka.OpenSearch.CancelServiceSoftwareUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a scheduled service software update for an Amazon OpenSearch
-- Service domain. You can only perform this operation before the
-- @AutomatedUpdateDate@ and when the domain\'s @UpdateStatus@ is
-- @PENDING_UPDATE@. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/service-software.html Service software updates in Amazon OpenSearch Service>.
module Amazonka.OpenSearch.CancelServiceSoftwareUpdate
  ( -- * Creating a Request
    CancelServiceSoftwareUpdate (..),
    newCancelServiceSoftwareUpdate,

    -- * Request Lenses
    cancelServiceSoftwareUpdate_domainName,

    -- * Destructuring the Response
    CancelServiceSoftwareUpdateResponse (..),
    newCancelServiceSoftwareUpdateResponse,

    -- * Response Lenses
    cancelServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    cancelServiceSoftwareUpdateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to cancel a service software
-- update.
--
-- /See:/ 'newCancelServiceSoftwareUpdate' smart constructor.
data CancelServiceSoftwareUpdate = CancelServiceSoftwareUpdate'
  { -- | Name of the OpenSearch Service domain that you want to cancel the
    -- service software update on.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelServiceSoftwareUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'cancelServiceSoftwareUpdate_domainName' - Name of the OpenSearch Service domain that you want to cancel the
-- service software update on.
newCancelServiceSoftwareUpdate ::
  -- | 'domainName'
  Prelude.Text ->
  CancelServiceSoftwareUpdate
newCancelServiceSoftwareUpdate pDomainName_ =
  CancelServiceSoftwareUpdate'
    { domainName =
        pDomainName_
    }

-- | Name of the OpenSearch Service domain that you want to cancel the
-- service software update on.
cancelServiceSoftwareUpdate_domainName :: Lens.Lens' CancelServiceSoftwareUpdate Prelude.Text
cancelServiceSoftwareUpdate_domainName = Lens.lens (\CancelServiceSoftwareUpdate' {domainName} -> domainName) (\s@CancelServiceSoftwareUpdate' {} a -> s {domainName = a} :: CancelServiceSoftwareUpdate)

instance Core.AWSRequest CancelServiceSoftwareUpdate where
  type
    AWSResponse CancelServiceSoftwareUpdate =
      CancelServiceSoftwareUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelServiceSoftwareUpdateResponse'
            Prelude.<$> (x Data..?> "ServiceSoftwareOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelServiceSoftwareUpdate where
  hashWithSalt _salt CancelServiceSoftwareUpdate' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData CancelServiceSoftwareUpdate where
  rnf CancelServiceSoftwareUpdate' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders CancelServiceSoftwareUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CancelServiceSoftwareUpdate where
  toJSON CancelServiceSoftwareUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath CancelServiceSoftwareUpdate where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/serviceSoftwareUpdate/cancel"

instance Data.ToQuery CancelServiceSoftwareUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response to a @CancelServiceSoftwareUpdate@ operation.
-- Contains the status of the update.
--
-- /See:/ 'newCancelServiceSoftwareUpdateResponse' smart constructor.
data CancelServiceSoftwareUpdateResponse = CancelServiceSoftwareUpdateResponse'
  { -- | Container for the state of your domain relative to the latest service
    -- software.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelServiceSoftwareUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSoftwareOptions', 'cancelServiceSoftwareUpdateResponse_serviceSoftwareOptions' - Container for the state of your domain relative to the latest service
-- software.
--
-- 'httpStatus', 'cancelServiceSoftwareUpdateResponse_httpStatus' - The response's http status code.
newCancelServiceSoftwareUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelServiceSoftwareUpdateResponse
newCancelServiceSoftwareUpdateResponse pHttpStatus_ =
  CancelServiceSoftwareUpdateResponse'
    { serviceSoftwareOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Container for the state of your domain relative to the latest service
-- software.
cancelServiceSoftwareUpdateResponse_serviceSoftwareOptions :: Lens.Lens' CancelServiceSoftwareUpdateResponse (Prelude.Maybe ServiceSoftwareOptions)
cancelServiceSoftwareUpdateResponse_serviceSoftwareOptions = Lens.lens (\CancelServiceSoftwareUpdateResponse' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@CancelServiceSoftwareUpdateResponse' {} a -> s {serviceSoftwareOptions = a} :: CancelServiceSoftwareUpdateResponse)

-- | The response's http status code.
cancelServiceSoftwareUpdateResponse_httpStatus :: Lens.Lens' CancelServiceSoftwareUpdateResponse Prelude.Int
cancelServiceSoftwareUpdateResponse_httpStatus = Lens.lens (\CancelServiceSoftwareUpdateResponse' {httpStatus} -> httpStatus) (\s@CancelServiceSoftwareUpdateResponse' {} a -> s {httpStatus = a} :: CancelServiceSoftwareUpdateResponse)

instance
  Prelude.NFData
    CancelServiceSoftwareUpdateResponse
  where
  rnf CancelServiceSoftwareUpdateResponse' {..} =
    Prelude.rnf serviceSoftwareOptions
      `Prelude.seq` Prelude.rnf httpStatus
