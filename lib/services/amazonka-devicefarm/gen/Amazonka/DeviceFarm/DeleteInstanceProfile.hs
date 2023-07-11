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
-- Module      : Amazonka.DeviceFarm.DeleteInstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profile that can be applied to one or more private device
-- instances.
module Amazonka.DeviceFarm.DeleteInstanceProfile
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInstanceProfile' smart constructor.
data DeleteInstanceProfile = DeleteInstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile you are
    -- requesting to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteInstanceProfile where
  type
    AWSResponse DeleteInstanceProfile =
      DeleteInstanceProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInstanceProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInstanceProfile where
  hashWithSalt _salt DeleteInstanceProfile' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteInstanceProfile where
  rnf DeleteInstanceProfile' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteInstanceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.DeleteInstanceProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteInstanceProfile where
  toJSON DeleteInstanceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteInstanceProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInstanceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceProfileResponse' smart constructor.
data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteInstanceProfileResponse where
  rnf DeleteInstanceProfileResponse' {..} =
    Prelude.rnf httpStatus
