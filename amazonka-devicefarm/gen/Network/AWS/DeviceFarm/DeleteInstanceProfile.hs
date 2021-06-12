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
-- Module      : Network.AWS.DeviceFarm.DeleteInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profile that can be applied to one or more private device
-- instances.
module Network.AWS.DeviceFarm.DeleteInstanceProfile
  ( -- * Creating a Request
    DeleteInstanceProfile (..),
    newDeleteInstanceProfile,

    -- * Request Lenses
    deleteInstanceProfile_arn,

    -- * Destructuring the Response
    DeleteInstanceProfileResponse (..),
    newDeleteInstanceProfileResponse,

    -- * Response Lenses
    deleteInstanceProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstanceProfile' smart constructor.
data DeleteInstanceProfile = DeleteInstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile you are
    -- requesting to delete.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteInstanceProfile_arn' - The Amazon Resource Name (ARN) of the instance profile you are
-- requesting to delete.
newDeleteInstanceProfile ::
  -- | 'arn'
  Core.Text ->
  DeleteInstanceProfile
newDeleteInstanceProfile pArn_ =
  DeleteInstanceProfile' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the instance profile you are
-- requesting to delete.
deleteInstanceProfile_arn :: Lens.Lens' DeleteInstanceProfile Core.Text
deleteInstanceProfile_arn = Lens.lens (\DeleteInstanceProfile' {arn} -> arn) (\s@DeleteInstanceProfile' {} a -> s {arn = a} :: DeleteInstanceProfile)

instance Core.AWSRequest DeleteInstanceProfile where
  type
    AWSResponse DeleteInstanceProfile =
      DeleteInstanceProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInstanceProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteInstanceProfile

instance Core.NFData DeleteInstanceProfile

instance Core.ToHeaders DeleteInstanceProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.DeleteInstanceProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteInstanceProfile where
  toJSON DeleteInstanceProfile' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath DeleteInstanceProfile where
  toPath = Core.const "/"

instance Core.ToQuery DeleteInstanceProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteInstanceProfileResponse' smart constructor.
data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteInstanceProfileResponse_httpStatus' - The response's http status code.
newDeleteInstanceProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteInstanceProfileResponse
newDeleteInstanceProfileResponse pHttpStatus_ =
  DeleteInstanceProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteInstanceProfileResponse_httpStatus :: Lens.Lens' DeleteInstanceProfileResponse Core.Int
deleteInstanceProfileResponse_httpStatus = Lens.lens (\DeleteInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceProfileResponse' {} a -> s {httpStatus = a} :: DeleteInstanceProfileResponse)

instance Core.NFData DeleteInstanceProfileResponse
