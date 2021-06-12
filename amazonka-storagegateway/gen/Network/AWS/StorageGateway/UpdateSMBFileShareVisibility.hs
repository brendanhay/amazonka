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
-- Module      : Network.AWS.StorageGateway.UpdateSMBFileShareVisibility
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Controls whether the shares on a gateway are visible in a net view or
-- browse list.
module Network.AWS.StorageGateway.UpdateSMBFileShareVisibility
  ( -- * Creating a Request
    UpdateSMBFileShareVisibility (..),
    newUpdateSMBFileShareVisibility,

    -- * Request Lenses
    updateSMBFileShareVisibility_gatewayARN,
    updateSMBFileShareVisibility_fileSharesVisible,

    -- * Destructuring the Response
    UpdateSMBFileShareVisibilityResponse (..),
    newUpdateSMBFileShareVisibilityResponse,

    -- * Response Lenses
    updateSMBFileShareVisibilityResponse_gatewayARN,
    updateSMBFileShareVisibilityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateSMBFileShareVisibility' smart constructor.
data UpdateSMBFileShareVisibility = UpdateSMBFileShareVisibility'
  { gatewayARN :: Core.Text,
    -- | The shares on this gateway appear when listing shares.
    fileSharesVisible :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSMBFileShareVisibility' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateSMBFileShareVisibility_gatewayARN' - Undocumented member.
--
-- 'fileSharesVisible', 'updateSMBFileShareVisibility_fileSharesVisible' - The shares on this gateway appear when listing shares.
newUpdateSMBFileShareVisibility ::
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'fileSharesVisible'
  Core.Bool ->
  UpdateSMBFileShareVisibility
newUpdateSMBFileShareVisibility
  pGatewayARN_
  pFileSharesVisible_ =
    UpdateSMBFileShareVisibility'
      { gatewayARN =
          pGatewayARN_,
        fileSharesVisible = pFileSharesVisible_
      }

-- | Undocumented member.
updateSMBFileShareVisibility_gatewayARN :: Lens.Lens' UpdateSMBFileShareVisibility Core.Text
updateSMBFileShareVisibility_gatewayARN = Lens.lens (\UpdateSMBFileShareVisibility' {gatewayARN} -> gatewayARN) (\s@UpdateSMBFileShareVisibility' {} a -> s {gatewayARN = a} :: UpdateSMBFileShareVisibility)

-- | The shares on this gateway appear when listing shares.
updateSMBFileShareVisibility_fileSharesVisible :: Lens.Lens' UpdateSMBFileShareVisibility Core.Bool
updateSMBFileShareVisibility_fileSharesVisible = Lens.lens (\UpdateSMBFileShareVisibility' {fileSharesVisible} -> fileSharesVisible) (\s@UpdateSMBFileShareVisibility' {} a -> s {fileSharesVisible = a} :: UpdateSMBFileShareVisibility)

instance Core.AWSRequest UpdateSMBFileShareVisibility where
  type
    AWSResponse UpdateSMBFileShareVisibility =
      UpdateSMBFileShareVisibilityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareVisibilityResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSMBFileShareVisibility

instance Core.NFData UpdateSMBFileShareVisibility

instance Core.ToHeaders UpdateSMBFileShareVisibility where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateSMBFileShareVisibility" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSMBFileShareVisibility where
  toJSON UpdateSMBFileShareVisibility' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just
              ("FileSharesVisible" Core..= fileSharesVisible)
          ]
      )

instance Core.ToPath UpdateSMBFileShareVisibility where
  toPath = Core.const "/"

instance Core.ToQuery UpdateSMBFileShareVisibility where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSMBFileShareVisibilityResponse' smart constructor.
data UpdateSMBFileShareVisibilityResponse = UpdateSMBFileShareVisibilityResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSMBFileShareVisibilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateSMBFileShareVisibilityResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateSMBFileShareVisibilityResponse_httpStatus' - The response's http status code.
newUpdateSMBFileShareVisibilityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateSMBFileShareVisibilityResponse
newUpdateSMBFileShareVisibilityResponse pHttpStatus_ =
  UpdateSMBFileShareVisibilityResponse'
    { gatewayARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateSMBFileShareVisibilityResponse_gatewayARN :: Lens.Lens' UpdateSMBFileShareVisibilityResponse (Core.Maybe Core.Text)
updateSMBFileShareVisibilityResponse_gatewayARN = Lens.lens (\UpdateSMBFileShareVisibilityResponse' {gatewayARN} -> gatewayARN) (\s@UpdateSMBFileShareVisibilityResponse' {} a -> s {gatewayARN = a} :: UpdateSMBFileShareVisibilityResponse)

-- | The response's http status code.
updateSMBFileShareVisibilityResponse_httpStatus :: Lens.Lens' UpdateSMBFileShareVisibilityResponse Core.Int
updateSMBFileShareVisibilityResponse_httpStatus = Lens.lens (\UpdateSMBFileShareVisibilityResponse' {httpStatus} -> httpStatus) (\s@UpdateSMBFileShareVisibilityResponse' {} a -> s {httpStatus = a} :: UpdateSMBFileShareVisibilityResponse)

instance
  Core.NFData
    UpdateSMBFileShareVisibilityResponse
