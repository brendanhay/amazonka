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
-- Module      : Network.AWS.Lightsail.AllocateStaticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a static IP address.
module Network.AWS.Lightsail.AllocateStaticIp
  ( -- * Creating a Request
    AllocateStaticIp (..),
    newAllocateStaticIp,

    -- * Request Lenses
    allocateStaticIp_staticIpName,

    -- * Destructuring the Response
    AllocateStaticIpResponse (..),
    newAllocateStaticIpResponse,

    -- * Response Lenses
    allocateStaticIpResponse_operations,
    allocateStaticIpResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAllocateStaticIp' smart constructor.
data AllocateStaticIp = AllocateStaticIp'
  { -- | The name of the static IP address.
    staticIpName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AllocateStaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIpName', 'allocateStaticIp_staticIpName' - The name of the static IP address.
newAllocateStaticIp ::
  -- | 'staticIpName'
  Core.Text ->
  AllocateStaticIp
newAllocateStaticIp pStaticIpName_ =
  AllocateStaticIp' {staticIpName = pStaticIpName_}

-- | The name of the static IP address.
allocateStaticIp_staticIpName :: Lens.Lens' AllocateStaticIp Core.Text
allocateStaticIp_staticIpName = Lens.lens (\AllocateStaticIp' {staticIpName} -> staticIpName) (\s@AllocateStaticIp' {} a -> s {staticIpName = a} :: AllocateStaticIp)

instance Core.AWSRequest AllocateStaticIp where
  type
    AWSResponse AllocateStaticIp =
      AllocateStaticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AllocateStaticIpResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AllocateStaticIp

instance Core.NFData AllocateStaticIp

instance Core.ToHeaders AllocateStaticIp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.AllocateStaticIp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AllocateStaticIp where
  toJSON AllocateStaticIp' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("staticIpName" Core..= staticIpName)]
      )

instance Core.ToPath AllocateStaticIp where
  toPath = Core.const "/"

instance Core.ToQuery AllocateStaticIp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAllocateStaticIpResponse' smart constructor.
data AllocateStaticIpResponse = AllocateStaticIpResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AllocateStaticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'allocateStaticIpResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'allocateStaticIpResponse_httpStatus' - The response's http status code.
newAllocateStaticIpResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AllocateStaticIpResponse
newAllocateStaticIpResponse pHttpStatus_ =
  AllocateStaticIpResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
allocateStaticIpResponse_operations :: Lens.Lens' AllocateStaticIpResponse (Core.Maybe [Operation])
allocateStaticIpResponse_operations = Lens.lens (\AllocateStaticIpResponse' {operations} -> operations) (\s@AllocateStaticIpResponse' {} a -> s {operations = a} :: AllocateStaticIpResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
allocateStaticIpResponse_httpStatus :: Lens.Lens' AllocateStaticIpResponse Core.Int
allocateStaticIpResponse_httpStatus = Lens.lens (\AllocateStaticIpResponse' {httpStatus} -> httpStatus) (\s@AllocateStaticIpResponse' {} a -> s {httpStatus = a} :: AllocateStaticIpResponse)

instance Core.NFData AllocateStaticIpResponse
