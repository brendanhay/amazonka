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
-- Module      : Amazonka.StorageGateway.ListLocalDisks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the gateway\'s local disks. To specify which gateway
-- to describe, you use the Amazon Resource Name (ARN) of the gateway in
-- the body of the request.
--
-- The request returns a list of all disks, specifying which are configured
-- as working storage, cache storage, or stored volume or not configured at
-- all. The response includes a @DiskStatus@ field. This field can have a
-- value of present (the disk is available to use), missing (the disk is no
-- longer connected to the gateway), or mismatch (the disk node is occupied
-- by a disk that has incorrect metadata or the disk content is corrupted).
module Amazonka.StorageGateway.ListLocalDisks
  ( -- * Creating a Request
    ListLocalDisks (..),
    newListLocalDisks,

    -- * Request Lenses
    listLocalDisks_gatewayARN,

    -- * Destructuring the Response
    ListLocalDisksResponse (..),
    newListLocalDisksResponse,

    -- * Response Lenses
    listLocalDisksResponse_disks,
    listLocalDisksResponse_gatewayARN,
    listLocalDisksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newListLocalDisks' smart constructor.
data ListLocalDisks = ListLocalDisks'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLocalDisks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'listLocalDisks_gatewayARN' - Undocumented member.
newListLocalDisks ::
  -- | 'gatewayARN'
  Prelude.Text ->
  ListLocalDisks
newListLocalDisks pGatewayARN_ =
  ListLocalDisks' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
listLocalDisks_gatewayARN :: Lens.Lens' ListLocalDisks Prelude.Text
listLocalDisks_gatewayARN = Lens.lens (\ListLocalDisks' {gatewayARN} -> gatewayARN) (\s@ListLocalDisks' {} a -> s {gatewayARN = a} :: ListLocalDisks)

instance Core.AWSRequest ListLocalDisks where
  type
    AWSResponse ListLocalDisks =
      ListLocalDisksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLocalDisksResponse'
            Prelude.<$> (x Core..?> "Disks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLocalDisks where
  hashWithSalt _salt ListLocalDisks' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData ListLocalDisks where
  rnf ListLocalDisks' {..} = Prelude.rnf gatewayARN

instance Core.ToHeaders ListLocalDisks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListLocalDisks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLocalDisks where
  toJSON ListLocalDisks' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath ListLocalDisks where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLocalDisks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLocalDisksResponse' smart constructor.
data ListLocalDisksResponse = ListLocalDisksResponse'
  { -- | A JSON object containing the following fields:
    --
    -- -   ListLocalDisksOutput$Disks
    disks :: Prelude.Maybe [Disk],
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLocalDisksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disks', 'listLocalDisksResponse_disks' - A JSON object containing the following fields:
--
-- -   ListLocalDisksOutput$Disks
--
-- 'gatewayARN', 'listLocalDisksResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'listLocalDisksResponse_httpStatus' - The response's http status code.
newListLocalDisksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLocalDisksResponse
newListLocalDisksResponse pHttpStatus_ =
  ListLocalDisksResponse'
    { disks = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON object containing the following fields:
--
-- -   ListLocalDisksOutput$Disks
listLocalDisksResponse_disks :: Lens.Lens' ListLocalDisksResponse (Prelude.Maybe [Disk])
listLocalDisksResponse_disks = Lens.lens (\ListLocalDisksResponse' {disks} -> disks) (\s@ListLocalDisksResponse' {} a -> s {disks = a} :: ListLocalDisksResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listLocalDisksResponse_gatewayARN :: Lens.Lens' ListLocalDisksResponse (Prelude.Maybe Prelude.Text)
listLocalDisksResponse_gatewayARN = Lens.lens (\ListLocalDisksResponse' {gatewayARN} -> gatewayARN) (\s@ListLocalDisksResponse' {} a -> s {gatewayARN = a} :: ListLocalDisksResponse)

-- | The response's http status code.
listLocalDisksResponse_httpStatus :: Lens.Lens' ListLocalDisksResponse Prelude.Int
listLocalDisksResponse_httpStatus = Lens.lens (\ListLocalDisksResponse' {httpStatus} -> httpStatus) (\s@ListLocalDisksResponse' {} a -> s {httpStatus = a} :: ListLocalDisksResponse)

instance Prelude.NFData ListLocalDisksResponse where
  rnf ListLocalDisksResponse' {..} =
    Prelude.rnf disks
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
