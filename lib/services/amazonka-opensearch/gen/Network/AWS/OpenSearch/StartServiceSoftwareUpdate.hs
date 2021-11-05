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
-- Module      : Network.AWS.OpenSearch.StartServiceSoftwareUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a service software update for an Amazon OpenSearch Service
-- domain.
module Network.AWS.OpenSearch.StartServiceSoftwareUpdate
  ( -- * Creating a Request
    StartServiceSoftwareUpdate (..),
    newStartServiceSoftwareUpdate,

    -- * Request Lenses
    startServiceSoftwareUpdate_domainName,

    -- * Destructuring the Response
    StartServiceSoftwareUpdateResponse (..),
    newStartServiceSoftwareUpdateResponse,

    -- * Response Lenses
    startServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startServiceSoftwareUpdateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @ StartServiceSoftwareUpdate @
-- operation. Specifies the name of the domain to schedule a service
-- software update for.
--
-- /See:/ 'newStartServiceSoftwareUpdate' smart constructor.
data StartServiceSoftwareUpdate = StartServiceSoftwareUpdate'
  { -- | The name of the domain that you want to update to the latest service
    -- software.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartServiceSoftwareUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'startServiceSoftwareUpdate_domainName' - The name of the domain that you want to update to the latest service
-- software.
newStartServiceSoftwareUpdate ::
  -- | 'domainName'
  Prelude.Text ->
  StartServiceSoftwareUpdate
newStartServiceSoftwareUpdate pDomainName_ =
  StartServiceSoftwareUpdate'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that you want to update to the latest service
-- software.
startServiceSoftwareUpdate_domainName :: Lens.Lens' StartServiceSoftwareUpdate Prelude.Text
startServiceSoftwareUpdate_domainName = Lens.lens (\StartServiceSoftwareUpdate' {domainName} -> domainName) (\s@StartServiceSoftwareUpdate' {} a -> s {domainName = a} :: StartServiceSoftwareUpdate)

instance Core.AWSRequest StartServiceSoftwareUpdate where
  type
    AWSResponse StartServiceSoftwareUpdate =
      StartServiceSoftwareUpdateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartServiceSoftwareUpdateResponse'
            Prelude.<$> (x Core..?> "ServiceSoftwareOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartServiceSoftwareUpdate

instance Prelude.NFData StartServiceSoftwareUpdate

instance Core.ToHeaders StartServiceSoftwareUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StartServiceSoftwareUpdate where
  toJSON StartServiceSoftwareUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Core..= domainName)]
      )

instance Core.ToPath StartServiceSoftwareUpdate where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/serviceSoftwareUpdate/start"

instance Core.ToQuery StartServiceSoftwareUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @StartServiceSoftwareUpdate@ operation. Contains the
-- status of the update.
--
-- /See:/ 'newStartServiceSoftwareUpdateResponse' smart constructor.
data StartServiceSoftwareUpdateResponse = StartServiceSoftwareUpdateResponse'
  { -- | The current status of the OpenSearch service software update.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartServiceSoftwareUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSoftwareOptions', 'startServiceSoftwareUpdateResponse_serviceSoftwareOptions' - The current status of the OpenSearch service software update.
--
-- 'httpStatus', 'startServiceSoftwareUpdateResponse_httpStatus' - The response's http status code.
newStartServiceSoftwareUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartServiceSoftwareUpdateResponse
newStartServiceSoftwareUpdateResponse pHttpStatus_ =
  StartServiceSoftwareUpdateResponse'
    { serviceSoftwareOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the OpenSearch service software update.
startServiceSoftwareUpdateResponse_serviceSoftwareOptions :: Lens.Lens' StartServiceSoftwareUpdateResponse (Prelude.Maybe ServiceSoftwareOptions)
startServiceSoftwareUpdateResponse_serviceSoftwareOptions = Lens.lens (\StartServiceSoftwareUpdateResponse' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@StartServiceSoftwareUpdateResponse' {} a -> s {serviceSoftwareOptions = a} :: StartServiceSoftwareUpdateResponse)

-- | The response's http status code.
startServiceSoftwareUpdateResponse_httpStatus :: Lens.Lens' StartServiceSoftwareUpdateResponse Prelude.Int
startServiceSoftwareUpdateResponse_httpStatus = Lens.lens (\StartServiceSoftwareUpdateResponse' {httpStatus} -> httpStatus) (\s@StartServiceSoftwareUpdateResponse' {} a -> s {httpStatus = a} :: StartServiceSoftwareUpdateResponse)

instance
  Prelude.NFData
    StartServiceSoftwareUpdateResponse
