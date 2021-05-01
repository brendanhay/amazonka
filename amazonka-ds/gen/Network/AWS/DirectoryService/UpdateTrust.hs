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
-- Module      : Network.AWS.DirectoryService.UpdateTrust
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the trust that has been set up between your AWS Managed
-- Microsoft AD directory and an on-premises Active Directory.
module Network.AWS.DirectoryService.UpdateTrust
  ( -- * Creating a Request
    UpdateTrust (..),
    newUpdateTrust,

    -- * Request Lenses
    updateTrust_selectiveAuth,
    updateTrust_trustId,

    -- * Destructuring the Response
    UpdateTrustResponse (..),
    newUpdateTrustResponse,

    -- * Response Lenses
    updateTrustResponse_trustId,
    updateTrustResponse_requestId,
    updateTrustResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTrust' smart constructor.
data UpdateTrust = UpdateTrust'
  { -- | Updates selective authentication for the trust.
    selectiveAuth :: Prelude.Maybe SelectiveAuth,
    -- | Identifier of the trust relationship.
    trustId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectiveAuth', 'updateTrust_selectiveAuth' - Updates selective authentication for the trust.
--
-- 'trustId', 'updateTrust_trustId' - Identifier of the trust relationship.
newUpdateTrust ::
  -- | 'trustId'
  Prelude.Text ->
  UpdateTrust
newUpdateTrust pTrustId_ =
  UpdateTrust'
    { selectiveAuth = Prelude.Nothing,
      trustId = pTrustId_
    }

-- | Updates selective authentication for the trust.
updateTrust_selectiveAuth :: Lens.Lens' UpdateTrust (Prelude.Maybe SelectiveAuth)
updateTrust_selectiveAuth = Lens.lens (\UpdateTrust' {selectiveAuth} -> selectiveAuth) (\s@UpdateTrust' {} a -> s {selectiveAuth = a} :: UpdateTrust)

-- | Identifier of the trust relationship.
updateTrust_trustId :: Lens.Lens' UpdateTrust Prelude.Text
updateTrust_trustId = Lens.lens (\UpdateTrust' {trustId} -> trustId) (\s@UpdateTrust' {} a -> s {trustId = a} :: UpdateTrust)

instance Prelude.AWSRequest UpdateTrust where
  type Rs UpdateTrust = UpdateTrustResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrustResponse'
            Prelude.<$> (x Prelude..?> "TrustId")
            Prelude.<*> (x Prelude..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrust

instance Prelude.NFData UpdateTrust

instance Prelude.ToHeaders UpdateTrust where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.UpdateTrust" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTrust where
  toJSON UpdateTrust' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SelectiveAuth" Prelude..=)
              Prelude.<$> selectiveAuth,
            Prelude.Just ("TrustId" Prelude..= trustId)
          ]
      )

instance Prelude.ToPath UpdateTrust where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTrust where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTrustResponse' smart constructor.
data UpdateTrustResponse = UpdateTrustResponse'
  { -- | Identifier of the trust relationship.
    trustId :: Prelude.Maybe Prelude.Text,
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrustResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustId', 'updateTrustResponse_trustId' - Identifier of the trust relationship.
--
-- 'requestId', 'updateTrustResponse_requestId' - Undocumented member.
--
-- 'httpStatus', 'updateTrustResponse_httpStatus' - The response's http status code.
newUpdateTrustResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTrustResponse
newUpdateTrustResponse pHttpStatus_ =
  UpdateTrustResponse'
    { trustId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier of the trust relationship.
updateTrustResponse_trustId :: Lens.Lens' UpdateTrustResponse (Prelude.Maybe Prelude.Text)
updateTrustResponse_trustId = Lens.lens (\UpdateTrustResponse' {trustId} -> trustId) (\s@UpdateTrustResponse' {} a -> s {trustId = a} :: UpdateTrustResponse)

-- | Undocumented member.
updateTrustResponse_requestId :: Lens.Lens' UpdateTrustResponse (Prelude.Maybe Prelude.Text)
updateTrustResponse_requestId = Lens.lens (\UpdateTrustResponse' {requestId} -> requestId) (\s@UpdateTrustResponse' {} a -> s {requestId = a} :: UpdateTrustResponse)

-- | The response's http status code.
updateTrustResponse_httpStatus :: Lens.Lens' UpdateTrustResponse Prelude.Int
updateTrustResponse_httpStatus = Lens.lens (\UpdateTrustResponse' {httpStatus} -> httpStatus) (\s@UpdateTrustResponse' {} a -> s {httpStatus = a} :: UpdateTrustResponse)

instance Prelude.NFData UpdateTrustResponse
