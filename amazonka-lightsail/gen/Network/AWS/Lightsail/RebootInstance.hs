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
-- Module      : Network.AWS.Lightsail.RebootInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific instance.
--
-- The @reboot instance@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @instance name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.RebootInstance
  ( -- * Creating a Request
    RebootInstance (..),
    newRebootInstance,

    -- * Request Lenses
    rebootInstance_instanceName,

    -- * Destructuring the Response
    RebootInstanceResponse (..),
    newRebootInstanceResponse,

    -- * Response Lenses
    rebootInstanceResponse_operations,
    rebootInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebootInstance' smart constructor.
data RebootInstance = RebootInstance'
  { -- | The name of the instance to reboot.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebootInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'rebootInstance_instanceName' - The name of the instance to reboot.
newRebootInstance ::
  -- | 'instanceName'
  Core.Text ->
  RebootInstance
newRebootInstance pInstanceName_ =
  RebootInstance' {instanceName = pInstanceName_}

-- | The name of the instance to reboot.
rebootInstance_instanceName :: Lens.Lens' RebootInstance Core.Text
rebootInstance_instanceName = Lens.lens (\RebootInstance' {instanceName} -> instanceName) (\s@RebootInstance' {} a -> s {instanceName = a} :: RebootInstance)

instance Core.AWSRequest RebootInstance where
  type
    AWSResponse RebootInstance =
      RebootInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootInstanceResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RebootInstance

instance Core.NFData RebootInstance

instance Core.ToHeaders RebootInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.RebootInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RebootInstance where
  toJSON RebootInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath RebootInstance where
  toPath = Core.const "/"

instance Core.ToQuery RebootInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RebootInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'rebootInstanceResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'rebootInstanceResponse_httpStatus' - The response's http status code.
newRebootInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RebootInstanceResponse
newRebootInstanceResponse pHttpStatus_ =
  RebootInstanceResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
rebootInstanceResponse_operations :: Lens.Lens' RebootInstanceResponse (Core.Maybe [Operation])
rebootInstanceResponse_operations = Lens.lens (\RebootInstanceResponse' {operations} -> operations) (\s@RebootInstanceResponse' {} a -> s {operations = a} :: RebootInstanceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
rebootInstanceResponse_httpStatus :: Lens.Lens' RebootInstanceResponse Core.Int
rebootInstanceResponse_httpStatus = Lens.lens (\RebootInstanceResponse' {httpStatus} -> httpStatus) (\s@RebootInstanceResponse' {} a -> s {httpStatus = a} :: RebootInstanceResponse)

instance Core.NFData RebootInstanceResponse
