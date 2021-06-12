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
-- Module      : Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the Access Status for AWS Organization portfolio share feature. This
-- API can only be called by the management account in the organization or
-- by a delegated admin.
module Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
  ( -- * Creating a Request
    GetAWSOrganizationsAccessStatus (..),
    newGetAWSOrganizationsAccessStatus,

    -- * Destructuring the Response
    GetAWSOrganizationsAccessStatusResponse (..),
    newGetAWSOrganizationsAccessStatusResponse,

    -- * Response Lenses
    getAWSOrganizationsAccessStatusResponse_accessStatus,
    getAWSOrganizationsAccessStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newGetAWSOrganizationsAccessStatus' smart constructor.
data GetAWSOrganizationsAccessStatus = GetAWSOrganizationsAccessStatus'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAWSOrganizationsAccessStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAWSOrganizationsAccessStatus ::
  GetAWSOrganizationsAccessStatus
newGetAWSOrganizationsAccessStatus =
  GetAWSOrganizationsAccessStatus'

instance
  Core.AWSRequest
    GetAWSOrganizationsAccessStatus
  where
  type
    AWSResponse GetAWSOrganizationsAccessStatus =
      GetAWSOrganizationsAccessStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAWSOrganizationsAccessStatusResponse'
            Core.<$> (x Core..?> "AccessStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetAWSOrganizationsAccessStatus

instance Core.NFData GetAWSOrganizationsAccessStatus

instance
  Core.ToHeaders
    GetAWSOrganizationsAccessStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.GetAWSOrganizationsAccessStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAWSOrganizationsAccessStatus where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetAWSOrganizationsAccessStatus where
  toPath = Core.const "/"

instance Core.ToQuery GetAWSOrganizationsAccessStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAWSOrganizationsAccessStatusResponse' smart constructor.
data GetAWSOrganizationsAccessStatusResponse = GetAWSOrganizationsAccessStatusResponse'
  { -- | The status of the portfolio share feature.
    accessStatus :: Core.Maybe AccessStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAWSOrganizationsAccessStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessStatus', 'getAWSOrganizationsAccessStatusResponse_accessStatus' - The status of the portfolio share feature.
--
-- 'httpStatus', 'getAWSOrganizationsAccessStatusResponse_httpStatus' - The response's http status code.
newGetAWSOrganizationsAccessStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAWSOrganizationsAccessStatusResponse
newGetAWSOrganizationsAccessStatusResponse
  pHttpStatus_ =
    GetAWSOrganizationsAccessStatusResponse'
      { accessStatus =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the portfolio share feature.
getAWSOrganizationsAccessStatusResponse_accessStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse (Core.Maybe AccessStatus)
getAWSOrganizationsAccessStatusResponse_accessStatus = Lens.lens (\GetAWSOrganizationsAccessStatusResponse' {accessStatus} -> accessStatus) (\s@GetAWSOrganizationsAccessStatusResponse' {} a -> s {accessStatus = a} :: GetAWSOrganizationsAccessStatusResponse)

-- | The response's http status code.
getAWSOrganizationsAccessStatusResponse_httpStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse Core.Int
getAWSOrganizationsAccessStatusResponse_httpStatus = Lens.lens (\GetAWSOrganizationsAccessStatusResponse' {httpStatus} -> httpStatus) (\s@GetAWSOrganizationsAccessStatusResponse' {} a -> s {httpStatus = a} :: GetAWSOrganizationsAccessStatusResponse)

instance
  Core.NFData
    GetAWSOrganizationsAccessStatusResponse
