{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateSMBFileShareVisibility' smart constructor.
data UpdateSMBFileShareVisibility = UpdateSMBFileShareVisibility'
  { gatewayARN :: Prelude.Text,
    -- | The shares on this gateway appear when listing shares.
    fileSharesVisible :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'fileSharesVisible'
  Prelude.Bool ->
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
updateSMBFileShareVisibility_gatewayARN :: Lens.Lens' UpdateSMBFileShareVisibility Prelude.Text
updateSMBFileShareVisibility_gatewayARN = Lens.lens (\UpdateSMBFileShareVisibility' {gatewayARN} -> gatewayARN) (\s@UpdateSMBFileShareVisibility' {} a -> s {gatewayARN = a} :: UpdateSMBFileShareVisibility)

-- | The shares on this gateway appear when listing shares.
updateSMBFileShareVisibility_fileSharesVisible :: Lens.Lens' UpdateSMBFileShareVisibility Prelude.Bool
updateSMBFileShareVisibility_fileSharesVisible = Lens.lens (\UpdateSMBFileShareVisibility' {fileSharesVisible} -> fileSharesVisible) (\s@UpdateSMBFileShareVisibility' {} a -> s {fileSharesVisible = a} :: UpdateSMBFileShareVisibility)

instance
  Prelude.AWSRequest
    UpdateSMBFileShareVisibility
  where
  type
    Rs UpdateSMBFileShareVisibility =
      UpdateSMBFileShareVisibilityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareVisibilityResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSMBFileShareVisibility

instance Prelude.NFData UpdateSMBFileShareVisibility

instance
  Prelude.ToHeaders
    UpdateSMBFileShareVisibility
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.UpdateSMBFileShareVisibility" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateSMBFileShareVisibility where
  toJSON UpdateSMBFileShareVisibility' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just
              ("FileSharesVisible" Prelude..= fileSharesVisible)
          ]
      )

instance Prelude.ToPath UpdateSMBFileShareVisibility where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateSMBFileShareVisibility where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSMBFileShareVisibilityResponse' smart constructor.
data UpdateSMBFileShareVisibilityResponse = UpdateSMBFileShareVisibilityResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateSMBFileShareVisibilityResponse
newUpdateSMBFileShareVisibilityResponse pHttpStatus_ =
  UpdateSMBFileShareVisibilityResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateSMBFileShareVisibilityResponse_gatewayARN :: Lens.Lens' UpdateSMBFileShareVisibilityResponse (Prelude.Maybe Prelude.Text)
updateSMBFileShareVisibilityResponse_gatewayARN = Lens.lens (\UpdateSMBFileShareVisibilityResponse' {gatewayARN} -> gatewayARN) (\s@UpdateSMBFileShareVisibilityResponse' {} a -> s {gatewayARN = a} :: UpdateSMBFileShareVisibilityResponse)

-- | The response's http status code.
updateSMBFileShareVisibilityResponse_httpStatus :: Lens.Lens' UpdateSMBFileShareVisibilityResponse Prelude.Int
updateSMBFileShareVisibilityResponse_httpStatus = Lens.lens (\UpdateSMBFileShareVisibilityResponse' {httpStatus} -> httpStatus) (\s@UpdateSMBFileShareVisibilityResponse' {} a -> s {httpStatus = a} :: UpdateSMBFileShareVisibilityResponse)

instance
  Prelude.NFData
    UpdateSMBFileShareVisibilityResponse
