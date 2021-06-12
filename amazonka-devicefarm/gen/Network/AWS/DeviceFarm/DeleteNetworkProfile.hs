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
-- Module      : Network.AWS.DeviceFarm.DeleteNetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network profile.
module Network.AWS.DeviceFarm.DeleteNetworkProfile
  ( -- * Creating a Request
    DeleteNetworkProfile (..),
    newDeleteNetworkProfile,

    -- * Request Lenses
    deleteNetworkProfile_arn,

    -- * Destructuring the Response
    DeleteNetworkProfileResponse (..),
    newDeleteNetworkProfileResponse,

    -- * Response Lenses
    deleteNetworkProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkProfile' smart constructor.
data DeleteNetworkProfile = DeleteNetworkProfile'
  { -- | The ARN of the network profile to delete.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteNetworkProfile_arn' - The ARN of the network profile to delete.
newDeleteNetworkProfile ::
  -- | 'arn'
  Core.Text ->
  DeleteNetworkProfile
newDeleteNetworkProfile pArn_ =
  DeleteNetworkProfile' {arn = pArn_}

-- | The ARN of the network profile to delete.
deleteNetworkProfile_arn :: Lens.Lens' DeleteNetworkProfile Core.Text
deleteNetworkProfile_arn = Lens.lens (\DeleteNetworkProfile' {arn} -> arn) (\s@DeleteNetworkProfile' {} a -> s {arn = a} :: DeleteNetworkProfile)

instance Core.AWSRequest DeleteNetworkProfile where
  type
    AWSResponse DeleteNetworkProfile =
      DeleteNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNetworkProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteNetworkProfile

instance Core.NFData DeleteNetworkProfile

instance Core.ToHeaders DeleteNetworkProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.DeleteNetworkProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteNetworkProfile where
  toJSON DeleteNetworkProfile' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath DeleteNetworkProfile where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNetworkProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteNetworkProfileResponse' smart constructor.
data DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNetworkProfileResponse_httpStatus' - The response's http status code.
newDeleteNetworkProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteNetworkProfileResponse
newDeleteNetworkProfileResponse pHttpStatus_ =
  DeleteNetworkProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteNetworkProfileResponse_httpStatus :: Lens.Lens' DeleteNetworkProfileResponse Core.Int
deleteNetworkProfileResponse_httpStatus = Lens.lens (\DeleteNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkProfileResponse' {} a -> s {httpStatus = a} :: DeleteNetworkProfileResponse)

instance Core.NFData DeleteNetworkProfileResponse
