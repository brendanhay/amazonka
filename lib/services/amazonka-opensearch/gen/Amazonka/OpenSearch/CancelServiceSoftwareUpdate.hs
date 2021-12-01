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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a scheduled service software update for an Amazon OpenSearch
-- Service domain. You can only perform this operation before the
-- @AutomatedUpdateDate@ and when the @UpdateStatus@ is in the
-- @PENDING_UPDATE@ state.
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
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @ CancelServiceSoftwareUpdate @
-- operation. Specifies the name of the domain that you wish to cancel a
-- service software update on.
--
-- /See:/ 'newCancelServiceSoftwareUpdate' smart constructor.
data CancelServiceSoftwareUpdate = CancelServiceSoftwareUpdate'
  { -- | The name of the domain that you want to stop the latest service software
    -- update on.
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
-- 'domainName', 'cancelServiceSoftwareUpdate_domainName' - The name of the domain that you want to stop the latest service software
-- update on.
newCancelServiceSoftwareUpdate ::
  -- | 'domainName'
  Prelude.Text ->
  CancelServiceSoftwareUpdate
newCancelServiceSoftwareUpdate pDomainName_ =
  CancelServiceSoftwareUpdate'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that you want to stop the latest service software
-- update on.
cancelServiceSoftwareUpdate_domainName :: Lens.Lens' CancelServiceSoftwareUpdate Prelude.Text
cancelServiceSoftwareUpdate_domainName = Lens.lens (\CancelServiceSoftwareUpdate' {domainName} -> domainName) (\s@CancelServiceSoftwareUpdate' {} a -> s {domainName = a} :: CancelServiceSoftwareUpdate)

instance Core.AWSRequest CancelServiceSoftwareUpdate where
  type
    AWSResponse CancelServiceSoftwareUpdate =
      CancelServiceSoftwareUpdateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelServiceSoftwareUpdateResponse'
            Prelude.<$> (x Core..?> "ServiceSoftwareOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelServiceSoftwareUpdate where
  hashWithSalt salt' CancelServiceSoftwareUpdate' {..} =
    salt' `Prelude.hashWithSalt` domainName

instance Prelude.NFData CancelServiceSoftwareUpdate where
  rnf CancelServiceSoftwareUpdate' {..} =
    Prelude.rnf domainName

instance Core.ToHeaders CancelServiceSoftwareUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CancelServiceSoftwareUpdate where
  toJSON CancelServiceSoftwareUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Core..= domainName)]
      )

instance Core.ToPath CancelServiceSoftwareUpdate where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/serviceSoftwareUpdate/cancel"

instance Core.ToQuery CancelServiceSoftwareUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @CancelServiceSoftwareUpdate@ operation. Contains the
-- status of the update.
--
-- /See:/ 'newCancelServiceSoftwareUpdateResponse' smart constructor.
data CancelServiceSoftwareUpdateResponse = CancelServiceSoftwareUpdateResponse'
  { -- | The current status of the OpenSearch service software update.
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
-- 'serviceSoftwareOptions', 'cancelServiceSoftwareUpdateResponse_serviceSoftwareOptions' - The current status of the OpenSearch service software update.
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

-- | The current status of the OpenSearch service software update.
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
