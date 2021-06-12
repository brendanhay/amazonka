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
-- Module      : Network.AWS.Lightsail.DetachStaticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a static IP from the Amazon Lightsail instance to which it is
-- attached.
module Network.AWS.Lightsail.DetachStaticIp
  ( -- * Creating a Request
    DetachStaticIp (..),
    newDetachStaticIp,

    -- * Request Lenses
    detachStaticIp_staticIpName,

    -- * Destructuring the Response
    DetachStaticIpResponse (..),
    newDetachStaticIpResponse,

    -- * Response Lenses
    detachStaticIpResponse_operations,
    detachStaticIpResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachStaticIp' smart constructor.
data DetachStaticIp = DetachStaticIp'
  { -- | The name of the static IP to detach from the instance.
    staticIpName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachStaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIpName', 'detachStaticIp_staticIpName' - The name of the static IP to detach from the instance.
newDetachStaticIp ::
  -- | 'staticIpName'
  Core.Text ->
  DetachStaticIp
newDetachStaticIp pStaticIpName_ =
  DetachStaticIp' {staticIpName = pStaticIpName_}

-- | The name of the static IP to detach from the instance.
detachStaticIp_staticIpName :: Lens.Lens' DetachStaticIp Core.Text
detachStaticIp_staticIpName = Lens.lens (\DetachStaticIp' {staticIpName} -> staticIpName) (\s@DetachStaticIp' {} a -> s {staticIpName = a} :: DetachStaticIp)

instance Core.AWSRequest DetachStaticIp where
  type
    AWSResponse DetachStaticIp =
      DetachStaticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachStaticIpResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachStaticIp

instance Core.NFData DetachStaticIp

instance Core.ToHeaders DetachStaticIp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DetachStaticIp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DetachStaticIp where
  toJSON DetachStaticIp' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("staticIpName" Core..= staticIpName)]
      )

instance Core.ToPath DetachStaticIp where
  toPath = Core.const "/"

instance Core.ToQuery DetachStaticIp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetachStaticIpResponse' smart constructor.
data DetachStaticIpResponse = DetachStaticIpResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachStaticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'detachStaticIpResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'detachStaticIpResponse_httpStatus' - The response's http status code.
newDetachStaticIpResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachStaticIpResponse
newDetachStaticIpResponse pHttpStatus_ =
  DetachStaticIpResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
detachStaticIpResponse_operations :: Lens.Lens' DetachStaticIpResponse (Core.Maybe [Operation])
detachStaticIpResponse_operations = Lens.lens (\DetachStaticIpResponse' {operations} -> operations) (\s@DetachStaticIpResponse' {} a -> s {operations = a} :: DetachStaticIpResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detachStaticIpResponse_httpStatus :: Lens.Lens' DetachStaticIpResponse Core.Int
detachStaticIpResponse_httpStatus = Lens.lens (\DetachStaticIpResponse' {httpStatus} -> httpStatus) (\s@DetachStaticIpResponse' {} a -> s {httpStatus = a} :: DetachStaticIpResponse)

instance Core.NFData DetachStaticIpResponse
