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
-- Module      : Network.AWS.ServiceCatalog.DescribeCopyProductStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified copy product operation.
module Network.AWS.ServiceCatalog.DescribeCopyProductStatus
  ( -- * Creating a Request
    DescribeCopyProductStatus (..),
    newDescribeCopyProductStatus,

    -- * Request Lenses
    describeCopyProductStatus_acceptLanguage,
    describeCopyProductStatus_copyProductToken,

    -- * Destructuring the Response
    DescribeCopyProductStatusResponse (..),
    newDescribeCopyProductStatusResponse,

    -- * Response Lenses
    describeCopyProductStatusResponse_statusDetail,
    describeCopyProductStatusResponse_targetProductId,
    describeCopyProductStatusResponse_copyProductStatus,
    describeCopyProductStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeCopyProductStatus' smart constructor.
data DescribeCopyProductStatus = DescribeCopyProductStatus'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The token for the copy product operation. This token is returned by
    -- CopyProduct.
    copyProductToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCopyProductStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeCopyProductStatus_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'copyProductToken', 'describeCopyProductStatus_copyProductToken' - The token for the copy product operation. This token is returned by
-- CopyProduct.
newDescribeCopyProductStatus ::
  -- | 'copyProductToken'
  Core.Text ->
  DescribeCopyProductStatus
newDescribeCopyProductStatus pCopyProductToken_ =
  DescribeCopyProductStatus'
    { acceptLanguage =
        Core.Nothing,
      copyProductToken = pCopyProductToken_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeCopyProductStatus_acceptLanguage :: Lens.Lens' DescribeCopyProductStatus (Core.Maybe Core.Text)
describeCopyProductStatus_acceptLanguage = Lens.lens (\DescribeCopyProductStatus' {acceptLanguage} -> acceptLanguage) (\s@DescribeCopyProductStatus' {} a -> s {acceptLanguage = a} :: DescribeCopyProductStatus)

-- | The token for the copy product operation. This token is returned by
-- CopyProduct.
describeCopyProductStatus_copyProductToken :: Lens.Lens' DescribeCopyProductStatus Core.Text
describeCopyProductStatus_copyProductToken = Lens.lens (\DescribeCopyProductStatus' {copyProductToken} -> copyProductToken) (\s@DescribeCopyProductStatus' {} a -> s {copyProductToken = a} :: DescribeCopyProductStatus)

instance Core.AWSRequest DescribeCopyProductStatus where
  type
    AWSResponse DescribeCopyProductStatus =
      DescribeCopyProductStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCopyProductStatusResponse'
            Core.<$> (x Core..?> "StatusDetail")
            Core.<*> (x Core..?> "TargetProductId")
            Core.<*> (x Core..?> "CopyProductStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCopyProductStatus

instance Core.NFData DescribeCopyProductStatus

instance Core.ToHeaders DescribeCopyProductStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeCopyProductStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCopyProductStatus where
  toJSON DescribeCopyProductStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just
              ("CopyProductToken" Core..= copyProductToken)
          ]
      )

instance Core.ToPath DescribeCopyProductStatus where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCopyProductStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCopyProductStatusResponse' smart constructor.
data DescribeCopyProductStatusResponse = DescribeCopyProductStatusResponse'
  { -- | The status message.
    statusDetail :: Core.Maybe Core.Text,
    -- | The identifier of the copied product.
    targetProductId :: Core.Maybe Core.Text,
    -- | The status of the copy product operation.
    copyProductStatus :: Core.Maybe CopyProductStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCopyProductStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusDetail', 'describeCopyProductStatusResponse_statusDetail' - The status message.
--
-- 'targetProductId', 'describeCopyProductStatusResponse_targetProductId' - The identifier of the copied product.
--
-- 'copyProductStatus', 'describeCopyProductStatusResponse_copyProductStatus' - The status of the copy product operation.
--
-- 'httpStatus', 'describeCopyProductStatusResponse_httpStatus' - The response's http status code.
newDescribeCopyProductStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCopyProductStatusResponse
newDescribeCopyProductStatusResponse pHttpStatus_ =
  DescribeCopyProductStatusResponse'
    { statusDetail =
        Core.Nothing,
      targetProductId = Core.Nothing,
      copyProductStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status message.
describeCopyProductStatusResponse_statusDetail :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Core.Text)
describeCopyProductStatusResponse_statusDetail = Lens.lens (\DescribeCopyProductStatusResponse' {statusDetail} -> statusDetail) (\s@DescribeCopyProductStatusResponse' {} a -> s {statusDetail = a} :: DescribeCopyProductStatusResponse)

-- | The identifier of the copied product.
describeCopyProductStatusResponse_targetProductId :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Core.Text)
describeCopyProductStatusResponse_targetProductId = Lens.lens (\DescribeCopyProductStatusResponse' {targetProductId} -> targetProductId) (\s@DescribeCopyProductStatusResponse' {} a -> s {targetProductId = a} :: DescribeCopyProductStatusResponse)

-- | The status of the copy product operation.
describeCopyProductStatusResponse_copyProductStatus :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe CopyProductStatus)
describeCopyProductStatusResponse_copyProductStatus = Lens.lens (\DescribeCopyProductStatusResponse' {copyProductStatus} -> copyProductStatus) (\s@DescribeCopyProductStatusResponse' {} a -> s {copyProductStatus = a} :: DescribeCopyProductStatusResponse)

-- | The response's http status code.
describeCopyProductStatusResponse_httpStatus :: Lens.Lens' DescribeCopyProductStatusResponse Core.Int
describeCopyProductStatusResponse_httpStatus = Lens.lens (\DescribeCopyProductStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeCopyProductStatusResponse' {} a -> s {httpStatus = a} :: DescribeCopyProductStatusResponse)

instance
  Core.NFData
    DescribeCopyProductStatusResponse
