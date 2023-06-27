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
-- Module      : Amazonka.Comprehend.DeleteFlywheel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a flywheel. When you delete the flywheel, Amazon Comprehend does
-- not delete the data lake or the model associated with the flywheel.
--
-- For more information about flywheels, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.DeleteFlywheel
  ( -- * Creating a Request
    DeleteFlywheel (..),
    newDeleteFlywheel,

    -- * Request Lenses
    deleteFlywheel_flywheelArn,

    -- * Destructuring the Response
    DeleteFlywheelResponse (..),
    newDeleteFlywheelResponse,

    -- * Response Lenses
    deleteFlywheelResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFlywheel' smart constructor.
data DeleteFlywheel = DeleteFlywheel'
  { -- | The Amazon Resource Number (ARN) of the flywheel to delete.
    flywheelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlywheel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelArn', 'deleteFlywheel_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel to delete.
newDeleteFlywheel ::
  -- | 'flywheelArn'
  Prelude.Text ->
  DeleteFlywheel
newDeleteFlywheel pFlywheelArn_ =
  DeleteFlywheel' {flywheelArn = pFlywheelArn_}

-- | The Amazon Resource Number (ARN) of the flywheel to delete.
deleteFlywheel_flywheelArn :: Lens.Lens' DeleteFlywheel Prelude.Text
deleteFlywheel_flywheelArn = Lens.lens (\DeleteFlywheel' {flywheelArn} -> flywheelArn) (\s@DeleteFlywheel' {} a -> s {flywheelArn = a} :: DeleteFlywheel)

instance Core.AWSRequest DeleteFlywheel where
  type
    AWSResponse DeleteFlywheel =
      DeleteFlywheelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFlywheelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFlywheel where
  hashWithSalt _salt DeleteFlywheel' {..} =
    _salt `Prelude.hashWithSalt` flywheelArn

instance Prelude.NFData DeleteFlywheel where
  rnf DeleteFlywheel' {..} = Prelude.rnf flywheelArn

instance Data.ToHeaders DeleteFlywheel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DeleteFlywheel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFlywheel where
  toJSON DeleteFlywheel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FlywheelArn" Data..= flywheelArn)]
      )

instance Data.ToPath DeleteFlywheel where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFlywheel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFlywheelResponse' smart constructor.
data DeleteFlywheelResponse = DeleteFlywheelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlywheelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFlywheelResponse_httpStatus' - The response's http status code.
newDeleteFlywheelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFlywheelResponse
newDeleteFlywheelResponse pHttpStatus_ =
  DeleteFlywheelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFlywheelResponse_httpStatus :: Lens.Lens' DeleteFlywheelResponse Prelude.Int
deleteFlywheelResponse_httpStatus = Lens.lens (\DeleteFlywheelResponse' {httpStatus} -> httpStatus) (\s@DeleteFlywheelResponse' {} a -> s {httpStatus = a} :: DeleteFlywheelResponse)

instance Prelude.NFData DeleteFlywheelResponse where
  rnf DeleteFlywheelResponse' {..} =
    Prelude.rnf httpStatus
