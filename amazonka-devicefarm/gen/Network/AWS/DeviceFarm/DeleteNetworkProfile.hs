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

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkProfile' smart constructor.
data DeleteNetworkProfile = DeleteNetworkProfile'
  { -- | The ARN of the network profile to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteNetworkProfile
newDeleteNetworkProfile pArn_ =
  DeleteNetworkProfile' {arn = pArn_}

-- | The ARN of the network profile to delete.
deleteNetworkProfile_arn :: Lens.Lens' DeleteNetworkProfile Prelude.Text
deleteNetworkProfile_arn = Lens.lens (\DeleteNetworkProfile' {arn} -> arn) (\s@DeleteNetworkProfile' {} a -> s {arn = a} :: DeleteNetworkProfile)

instance Prelude.AWSRequest DeleteNetworkProfile where
  type
    Rs DeleteNetworkProfile =
      DeleteNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNetworkProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNetworkProfile

instance Prelude.NFData DeleteNetworkProfile

instance Prelude.ToHeaders DeleteNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.DeleteNetworkProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteNetworkProfile where
  toJSON DeleteNetworkProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath DeleteNetworkProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteNetworkProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNetworkProfileResponse' smart constructor.
data DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteNetworkProfileResponse
newDeleteNetworkProfileResponse pHttpStatus_ =
  DeleteNetworkProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteNetworkProfileResponse_httpStatus :: Lens.Lens' DeleteNetworkProfileResponse Prelude.Int
deleteNetworkProfileResponse_httpStatus = Lens.lens (\DeleteNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkProfileResponse' {} a -> s {httpStatus = a} :: DeleteNetworkProfileResponse)

instance Prelude.NFData DeleteNetworkProfileResponse
