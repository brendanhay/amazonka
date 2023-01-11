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
-- Module      : Amazonka.RDS.DeleteBlueGreenDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a blue\/green deployment.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDS.DeleteBlueGreenDeployment
  ( -- * Creating a Request
    DeleteBlueGreenDeployment (..),
    newDeleteBlueGreenDeployment,

    -- * Request Lenses
    deleteBlueGreenDeployment_deleteTarget,
    deleteBlueGreenDeployment_blueGreenDeploymentIdentifier,

    -- * Destructuring the Response
    DeleteBlueGreenDeploymentResponse (..),
    newDeleteBlueGreenDeploymentResponse,

    -- * Response Lenses
    deleteBlueGreenDeploymentResponse_blueGreenDeployment,
    deleteBlueGreenDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBlueGreenDeployment' smart constructor.
data DeleteBlueGreenDeployment = DeleteBlueGreenDeployment'
  { -- | A value that indicates whether to delete the resources in the green
    -- environment.
    deleteTarget :: Prelude.Maybe Prelude.Bool,
    -- | The blue\/green deployment identifier of the deployment to be deleted.
    -- This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match an existing blue\/green deployment identifier.
    blueGreenDeploymentIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBlueGreenDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteTarget', 'deleteBlueGreenDeployment_deleteTarget' - A value that indicates whether to delete the resources in the green
-- environment.
--
-- 'blueGreenDeploymentIdentifier', 'deleteBlueGreenDeployment_blueGreenDeploymentIdentifier' - The blue\/green deployment identifier of the deployment to be deleted.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing blue\/green deployment identifier.
newDeleteBlueGreenDeployment ::
  -- | 'blueGreenDeploymentIdentifier'
  Prelude.Text ->
  DeleteBlueGreenDeployment
newDeleteBlueGreenDeployment
  pBlueGreenDeploymentIdentifier_ =
    DeleteBlueGreenDeployment'
      { deleteTarget =
          Prelude.Nothing,
        blueGreenDeploymentIdentifier =
          pBlueGreenDeploymentIdentifier_
      }

-- | A value that indicates whether to delete the resources in the green
-- environment.
deleteBlueGreenDeployment_deleteTarget :: Lens.Lens' DeleteBlueGreenDeployment (Prelude.Maybe Prelude.Bool)
deleteBlueGreenDeployment_deleteTarget = Lens.lens (\DeleteBlueGreenDeployment' {deleteTarget} -> deleteTarget) (\s@DeleteBlueGreenDeployment' {} a -> s {deleteTarget = a} :: DeleteBlueGreenDeployment)

-- | The blue\/green deployment identifier of the deployment to be deleted.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing blue\/green deployment identifier.
deleteBlueGreenDeployment_blueGreenDeploymentIdentifier :: Lens.Lens' DeleteBlueGreenDeployment Prelude.Text
deleteBlueGreenDeployment_blueGreenDeploymentIdentifier = Lens.lens (\DeleteBlueGreenDeployment' {blueGreenDeploymentIdentifier} -> blueGreenDeploymentIdentifier) (\s@DeleteBlueGreenDeployment' {} a -> s {blueGreenDeploymentIdentifier = a} :: DeleteBlueGreenDeployment)

instance Core.AWSRequest DeleteBlueGreenDeployment where
  type
    AWSResponse DeleteBlueGreenDeployment =
      DeleteBlueGreenDeploymentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteBlueGreenDeploymentResult"
      ( \s h x ->
          DeleteBlueGreenDeploymentResponse'
            Prelude.<$> (x Data..@? "BlueGreenDeployment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBlueGreenDeployment where
  hashWithSalt _salt DeleteBlueGreenDeployment' {..} =
    _salt `Prelude.hashWithSalt` deleteTarget
      `Prelude.hashWithSalt` blueGreenDeploymentIdentifier

instance Prelude.NFData DeleteBlueGreenDeployment where
  rnf DeleteBlueGreenDeployment' {..} =
    Prelude.rnf deleteTarget
      `Prelude.seq` Prelude.rnf blueGreenDeploymentIdentifier

instance Data.ToHeaders DeleteBlueGreenDeployment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteBlueGreenDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBlueGreenDeployment where
  toQuery DeleteBlueGreenDeployment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteBlueGreenDeployment" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DeleteTarget" Data.=: deleteTarget,
        "BlueGreenDeploymentIdentifier"
          Data.=: blueGreenDeploymentIdentifier
      ]

-- | /See:/ 'newDeleteBlueGreenDeploymentResponse' smart constructor.
data DeleteBlueGreenDeploymentResponse = DeleteBlueGreenDeploymentResponse'
  { blueGreenDeployment :: Prelude.Maybe BlueGreenDeployment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBlueGreenDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueGreenDeployment', 'deleteBlueGreenDeploymentResponse_blueGreenDeployment' - Undocumented member.
--
-- 'httpStatus', 'deleteBlueGreenDeploymentResponse_httpStatus' - The response's http status code.
newDeleteBlueGreenDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBlueGreenDeploymentResponse
newDeleteBlueGreenDeploymentResponse pHttpStatus_ =
  DeleteBlueGreenDeploymentResponse'
    { blueGreenDeployment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteBlueGreenDeploymentResponse_blueGreenDeployment :: Lens.Lens' DeleteBlueGreenDeploymentResponse (Prelude.Maybe BlueGreenDeployment)
deleteBlueGreenDeploymentResponse_blueGreenDeployment = Lens.lens (\DeleteBlueGreenDeploymentResponse' {blueGreenDeployment} -> blueGreenDeployment) (\s@DeleteBlueGreenDeploymentResponse' {} a -> s {blueGreenDeployment = a} :: DeleteBlueGreenDeploymentResponse)

-- | The response's http status code.
deleteBlueGreenDeploymentResponse_httpStatus :: Lens.Lens' DeleteBlueGreenDeploymentResponse Prelude.Int
deleteBlueGreenDeploymentResponse_httpStatus = Lens.lens (\DeleteBlueGreenDeploymentResponse' {httpStatus} -> httpStatus) (\s@DeleteBlueGreenDeploymentResponse' {} a -> s {httpStatus = a} :: DeleteBlueGreenDeploymentResponse)

instance
  Prelude.NFData
    DeleteBlueGreenDeploymentResponse
  where
  rnf DeleteBlueGreenDeploymentResponse' {..} =
    Prelude.rnf blueGreenDeployment
      `Prelude.seq` Prelude.rnf httpStatus
