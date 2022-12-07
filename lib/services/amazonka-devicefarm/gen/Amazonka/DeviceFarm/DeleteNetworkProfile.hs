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
-- Module      : Amazonka.DeviceFarm.DeleteNetworkProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network profile.
module Amazonka.DeviceFarm.DeleteNetworkProfile
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetworkProfile' smart constructor.
data DeleteNetworkProfile = DeleteNetworkProfile'
  { -- | The ARN of the network profile to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteNetworkProfile where
  type
    AWSResponse DeleteNetworkProfile =
      DeleteNetworkProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNetworkProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNetworkProfile where
  hashWithSalt _salt DeleteNetworkProfile' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteNetworkProfile where
  rnf DeleteNetworkProfile' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.DeleteNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteNetworkProfile where
  toJSON DeleteNetworkProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteNetworkProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNetworkProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNetworkProfileResponse' smart constructor.
data DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteNetworkProfileResponse where
  rnf DeleteNetworkProfileResponse' {..} =
    Prelude.rnf httpStatus
