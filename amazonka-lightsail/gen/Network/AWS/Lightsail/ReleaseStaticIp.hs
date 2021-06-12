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
-- Module      : Network.AWS.Lightsail.ReleaseStaticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific static IP from your account.
module Network.AWS.Lightsail.ReleaseStaticIp
  ( -- * Creating a Request
    ReleaseStaticIp (..),
    newReleaseStaticIp,

    -- * Request Lenses
    releaseStaticIp_staticIpName,

    -- * Destructuring the Response
    ReleaseStaticIpResponse (..),
    newReleaseStaticIpResponse,

    -- * Response Lenses
    releaseStaticIpResponse_operations,
    releaseStaticIpResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReleaseStaticIp' smart constructor.
data ReleaseStaticIp = ReleaseStaticIp'
  { -- | The name of the static IP to delete.
    staticIpName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReleaseStaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIpName', 'releaseStaticIp_staticIpName' - The name of the static IP to delete.
newReleaseStaticIp ::
  -- | 'staticIpName'
  Core.Text ->
  ReleaseStaticIp
newReleaseStaticIp pStaticIpName_ =
  ReleaseStaticIp' {staticIpName = pStaticIpName_}

-- | The name of the static IP to delete.
releaseStaticIp_staticIpName :: Lens.Lens' ReleaseStaticIp Core.Text
releaseStaticIp_staticIpName = Lens.lens (\ReleaseStaticIp' {staticIpName} -> staticIpName) (\s@ReleaseStaticIp' {} a -> s {staticIpName = a} :: ReleaseStaticIp)

instance Core.AWSRequest ReleaseStaticIp where
  type
    AWSResponse ReleaseStaticIp =
      ReleaseStaticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ReleaseStaticIpResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ReleaseStaticIp

instance Core.NFData ReleaseStaticIp

instance Core.ToHeaders ReleaseStaticIp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.ReleaseStaticIp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ReleaseStaticIp where
  toJSON ReleaseStaticIp' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("staticIpName" Core..= staticIpName)]
      )

instance Core.ToPath ReleaseStaticIp where
  toPath = Core.const "/"

instance Core.ToQuery ReleaseStaticIp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newReleaseStaticIpResponse' smart constructor.
data ReleaseStaticIpResponse = ReleaseStaticIpResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReleaseStaticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'releaseStaticIpResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'releaseStaticIpResponse_httpStatus' - The response's http status code.
newReleaseStaticIpResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ReleaseStaticIpResponse
newReleaseStaticIpResponse pHttpStatus_ =
  ReleaseStaticIpResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
releaseStaticIpResponse_operations :: Lens.Lens' ReleaseStaticIpResponse (Core.Maybe [Operation])
releaseStaticIpResponse_operations = Lens.lens (\ReleaseStaticIpResponse' {operations} -> operations) (\s@ReleaseStaticIpResponse' {} a -> s {operations = a} :: ReleaseStaticIpResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
releaseStaticIpResponse_httpStatus :: Lens.Lens' ReleaseStaticIpResponse Core.Int
releaseStaticIpResponse_httpStatus = Lens.lens (\ReleaseStaticIpResponse' {httpStatus} -> httpStatus) (\s@ReleaseStaticIpResponse' {} a -> s {httpStatus = a} :: ReleaseStaticIpResponse)

instance Core.NFData ReleaseStaticIpResponse
