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
-- Module      : Network.AWS.DeviceFarm.DeleteDevicePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device pool given the pool ARN. Does not allow deletion of
-- curated pools owned by the system.
module Network.AWS.DeviceFarm.DeleteDevicePool
  ( -- * Creating a Request
    DeleteDevicePool (..),
    newDeleteDevicePool,

    -- * Request Lenses
    deleteDevicePool_arn,

    -- * Destructuring the Response
    DeleteDevicePoolResponse (..),
    newDeleteDevicePoolResponse,

    -- * Response Lenses
    deleteDevicePoolResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete device pool operation.
--
-- /See:/ 'newDeleteDevicePool' smart constructor.
data DeleteDevicePool = DeleteDevicePool'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool
    -- to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteDevicePool_arn' - Represents the Amazon Resource Name (ARN) of the Device Farm device pool
-- to delete.
newDeleteDevicePool ::
  -- | 'arn'
  Prelude.Text ->
  DeleteDevicePool
newDeleteDevicePool pArn_ =
  DeleteDevicePool' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool
-- to delete.
deleteDevicePool_arn :: Lens.Lens' DeleteDevicePool Prelude.Text
deleteDevicePool_arn = Lens.lens (\DeleteDevicePool' {arn} -> arn) (\s@DeleteDevicePool' {} a -> s {arn = a} :: DeleteDevicePool)

instance Prelude.AWSRequest DeleteDevicePool where
  type Rs DeleteDevicePool = DeleteDevicePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDevicePoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDevicePool

instance Prelude.NFData DeleteDevicePool

instance Prelude.ToHeaders DeleteDevicePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.DeleteDevicePool" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDevicePool where
  toJSON DeleteDevicePool' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath DeleteDevicePool where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDevicePool where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a delete device pool request.
--
-- /See:/ 'newDeleteDevicePoolResponse' smart constructor.
data DeleteDevicePoolResponse = DeleteDevicePoolResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDevicePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDevicePoolResponse_httpStatus' - The response's http status code.
newDeleteDevicePoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDevicePoolResponse
newDeleteDevicePoolResponse pHttpStatus_ =
  DeleteDevicePoolResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDevicePoolResponse_httpStatus :: Lens.Lens' DeleteDevicePoolResponse Prelude.Int
deleteDevicePoolResponse_httpStatus = Lens.lens (\DeleteDevicePoolResponse' {httpStatus} -> httpStatus) (\s@DeleteDevicePoolResponse' {} a -> s {httpStatus = a} :: DeleteDevicePoolResponse)

instance Prelude.NFData DeleteDevicePoolResponse
