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
-- Module      : Network.AWS.Lightsail.DeleteInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Lightsail instance.
--
-- The @delete instance@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @instance name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteInstance
  ( -- * Creating a Request
    DeleteInstance (..),
    newDeleteInstance,

    -- * Request Lenses
    deleteInstance_forceDeleteAddOns,
    deleteInstance_instanceName,

    -- * Destructuring the Response
    DeleteInstanceResponse (..),
    newDeleteInstanceResponse,

    -- * Response Lenses
    deleteInstanceResponse_operations,
    deleteInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { -- | A Boolean value to indicate whether to delete the enabled add-ons for
    -- the disk.
    forceDeleteAddOns :: Core.Maybe Core.Bool,
    -- | The name of the instance to delete.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDeleteAddOns', 'deleteInstance_forceDeleteAddOns' - A Boolean value to indicate whether to delete the enabled add-ons for
-- the disk.
--
-- 'instanceName', 'deleteInstance_instanceName' - The name of the instance to delete.
newDeleteInstance ::
  -- | 'instanceName'
  Core.Text ->
  DeleteInstance
newDeleteInstance pInstanceName_ =
  DeleteInstance'
    { forceDeleteAddOns = Core.Nothing,
      instanceName = pInstanceName_
    }

-- | A Boolean value to indicate whether to delete the enabled add-ons for
-- the disk.
deleteInstance_forceDeleteAddOns :: Lens.Lens' DeleteInstance (Core.Maybe Core.Bool)
deleteInstance_forceDeleteAddOns = Lens.lens (\DeleteInstance' {forceDeleteAddOns} -> forceDeleteAddOns) (\s@DeleteInstance' {} a -> s {forceDeleteAddOns = a} :: DeleteInstance)

-- | The name of the instance to delete.
deleteInstance_instanceName :: Lens.Lens' DeleteInstance Core.Text
deleteInstance_instanceName = Lens.lens (\DeleteInstance' {instanceName} -> instanceName) (\s@DeleteInstance' {} a -> s {instanceName = a} :: DeleteInstance)

instance Core.AWSRequest DeleteInstance where
  type
    AWSResponse DeleteInstance =
      DeleteInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInstanceResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteInstance

instance Core.NFData DeleteInstance

instance Core.ToHeaders DeleteInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteInstance where
  toJSON DeleteInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("forceDeleteAddOns" Core..=)
              Core.<$> forceDeleteAddOns,
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.ToPath DeleteInstance where
  toPath = Core.const "/"

instance Core.ToQuery DeleteInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteInstanceResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteInstanceResponse_httpStatus' - The response's http status code.
newDeleteInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteInstanceResponse
newDeleteInstanceResponse pHttpStatus_ =
  DeleteInstanceResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteInstanceResponse_operations :: Lens.Lens' DeleteInstanceResponse (Core.Maybe [Operation])
deleteInstanceResponse_operations = Lens.lens (\DeleteInstanceResponse' {operations} -> operations) (\s@DeleteInstanceResponse' {} a -> s {operations = a} :: DeleteInstanceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteInstanceResponse_httpStatus :: Lens.Lens' DeleteInstanceResponse Core.Int
deleteInstanceResponse_httpStatus = Lens.lens (\DeleteInstanceResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceResponse' {} a -> s {httpStatus = a} :: DeleteInstanceResponse)

instance Core.NFData DeleteInstanceResponse
