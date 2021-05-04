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

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstanceProfile' smart constructor.
data DeleteInstanceProfile = DeleteInstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile you are
    -- requesting to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteInstanceProfile
newDeleteInstanceProfile pArn_ =
  DeleteInstanceProfile' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the instance profile you are
-- requesting to delete.
deleteInstanceProfile_arn :: Lens.Lens' DeleteInstanceProfile Prelude.Text
deleteInstanceProfile_arn = Lens.lens (\DeleteInstanceProfile' {arn} -> arn) (\s@DeleteInstanceProfile' {} a -> s {arn = a} :: DeleteInstanceProfile)

instance Prelude.AWSRequest DeleteInstanceProfile where
  type
    Rs DeleteInstanceProfile =
      DeleteInstanceProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInstanceProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInstanceProfile

instance Prelude.NFData DeleteInstanceProfile

instance Prelude.ToHeaders DeleteInstanceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.DeleteInstanceProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteInstanceProfile where
  toJSON DeleteInstanceProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath DeleteInstanceProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteInstanceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceProfileResponse' smart constructor.
data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteInstanceProfileResponse
newDeleteInstanceProfileResponse pHttpStatus_ =
  DeleteInstanceProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteInstanceProfileResponse_httpStatus :: Lens.Lens' DeleteInstanceProfileResponse Prelude.Int
deleteInstanceProfileResponse_httpStatus = Lens.lens (\DeleteInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceProfileResponse' {} a -> s {httpStatus = a} :: DeleteInstanceProfileResponse)

instance Prelude.NFData DeleteInstanceProfileResponse
