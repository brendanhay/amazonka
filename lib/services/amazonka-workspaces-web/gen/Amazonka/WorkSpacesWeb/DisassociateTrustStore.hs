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
-- Module      : Amazonka.WorkSpacesWeb.DisassociateTrustStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a trust store from a web portal.
module Amazonka.WorkSpacesWeb.DisassociateTrustStore
  ( -- * Creating a Request
    DisassociateTrustStore (..),
    newDisassociateTrustStore,

    -- * Request Lenses
    disassociateTrustStore_portalArn,

    -- * Destructuring the Response
    DisassociateTrustStoreResponse (..),
    newDisassociateTrustStoreResponse,

    -- * Response Lenses
    disassociateTrustStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDisassociateTrustStore' smart constructor.
data DisassociateTrustStore = DisassociateTrustStore'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTrustStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'disassociateTrustStore_portalArn' - The ARN of the web portal.
newDisassociateTrustStore ::
  -- | 'portalArn'
  Prelude.Text ->
  DisassociateTrustStore
newDisassociateTrustStore pPortalArn_ =
  DisassociateTrustStore' {portalArn = pPortalArn_}

-- | The ARN of the web portal.
disassociateTrustStore_portalArn :: Lens.Lens' DisassociateTrustStore Prelude.Text
disassociateTrustStore_portalArn = Lens.lens (\DisassociateTrustStore' {portalArn} -> portalArn) (\s@DisassociateTrustStore' {} a -> s {portalArn = a} :: DisassociateTrustStore)

instance Core.AWSRequest DisassociateTrustStore where
  type
    AWSResponse DisassociateTrustStore =
      DisassociateTrustStoreResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateTrustStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateTrustStore where
  hashWithSalt _salt DisassociateTrustStore' {..} =
    _salt `Prelude.hashWithSalt` portalArn

instance Prelude.NFData DisassociateTrustStore where
  rnf DisassociateTrustStore' {..} =
    Prelude.rnf portalArn

instance Core.ToHeaders DisassociateTrustStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DisassociateTrustStore where
  toPath DisassociateTrustStore' {..} =
    Prelude.mconcat
      ["/portals/", Core.toBS portalArn, "/trustStores"]

instance Core.ToQuery DisassociateTrustStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateTrustStoreResponse' smart constructor.
data DisassociateTrustStoreResponse = DisassociateTrustStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTrustStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateTrustStoreResponse_httpStatus' - The response's http status code.
newDisassociateTrustStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTrustStoreResponse
newDisassociateTrustStoreResponse pHttpStatus_ =
  DisassociateTrustStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateTrustStoreResponse_httpStatus :: Lens.Lens' DisassociateTrustStoreResponse Prelude.Int
disassociateTrustStoreResponse_httpStatus = Lens.lens (\DisassociateTrustStoreResponse' {httpStatus} -> httpStatus) (\s@DisassociateTrustStoreResponse' {} a -> s {httpStatus = a} :: DisassociateTrustStoreResponse)

instance
  Prelude.NFData
    DisassociateTrustStoreResponse
  where
  rnf DisassociateTrustStoreResponse' {..} =
    Prelude.rnf httpStatus
