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
-- Module      : Network.AWS.StorageGateway.ListLocalDisks
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.StorageGateway.ListLocalDisks
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newListLocalDisks' smart constructor.
data ListLocalDisks = ListLocalDisks'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListLocalDisks
newListLocalDisks pGatewayARN_ =
  ListLocalDisks' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
listLocalDisks_gatewayARN :: Lens.Lens' ListLocalDisks Core.Text
listLocalDisks_gatewayARN = Lens.lens (\ListLocalDisks' {gatewayARN} -> gatewayARN) (\s@ListLocalDisks' {} a -> s {gatewayARN = a} :: ListLocalDisks)

instance Core.AWSRequest ListLocalDisks where
  type
    AWSResponse ListLocalDisks =
      ListLocalDisksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLocalDisksResponse'
            Core.<$> (x Core..?> "Disks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListLocalDisks

instance Core.NFData ListLocalDisks

instance Core.ToHeaders ListLocalDisks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ListLocalDisks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListLocalDisks where
  toJSON ListLocalDisks' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath ListLocalDisks where
  toPath = Core.const "/"

instance Core.ToQuery ListLocalDisks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListLocalDisksResponse' smart constructor.
data ListLocalDisksResponse = ListLocalDisksResponse'
  { -- | A JSON object containing the following fields:
    --
    -- -   ListLocalDisksOutput$Disks
    disks :: Core.Maybe [Disk],
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListLocalDisksResponse
newListLocalDisksResponse pHttpStatus_ =
  ListLocalDisksResponse'
    { disks = Core.Nothing,
      gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON object containing the following fields:
--
-- -   ListLocalDisksOutput$Disks
listLocalDisksResponse_disks :: Lens.Lens' ListLocalDisksResponse (Core.Maybe [Disk])
listLocalDisksResponse_disks = Lens.lens (\ListLocalDisksResponse' {disks} -> disks) (\s@ListLocalDisksResponse' {} a -> s {disks = a} :: ListLocalDisksResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
listLocalDisksResponse_gatewayARN :: Lens.Lens' ListLocalDisksResponse (Core.Maybe Core.Text)
listLocalDisksResponse_gatewayARN = Lens.lens (\ListLocalDisksResponse' {gatewayARN} -> gatewayARN) (\s@ListLocalDisksResponse' {} a -> s {gatewayARN = a} :: ListLocalDisksResponse)

-- | The response's http status code.
listLocalDisksResponse_httpStatus :: Lens.Lens' ListLocalDisksResponse Core.Int
listLocalDisksResponse_httpStatus = Lens.lens (\ListLocalDisksResponse' {httpStatus} -> httpStatus) (\s@ListLocalDisksResponse' {} a -> s {httpStatus = a} :: ListLocalDisksResponse)

instance Core.NFData ListLocalDisksResponse
