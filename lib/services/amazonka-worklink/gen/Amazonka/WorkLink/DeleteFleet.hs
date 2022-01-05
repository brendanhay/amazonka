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
-- Module      : Amazonka.WorkLink.DeleteFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet. Prevents users from accessing previously associated
-- websites.
module Amazonka.WorkLink.DeleteFleet
  ( -- * Creating a Request
    DeleteFleet (..),
    newDeleteFleet,

    -- * Request Lenses
    deleteFleet_fleetArn,

    -- * Destructuring the Response
    DeleteFleetResponse (..),
    newDeleteFleetResponse,

    -- * Response Lenses
    deleteFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDeleteFleet' smart constructor.
data DeleteFleet = DeleteFleet'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'deleteFleet_fleetArn' - The ARN of the fleet.
newDeleteFleet ::
  -- | 'fleetArn'
  Prelude.Text ->
  DeleteFleet
newDeleteFleet pFleetArn_ =
  DeleteFleet' {fleetArn = pFleetArn_}

-- | The ARN of the fleet.
deleteFleet_fleetArn :: Lens.Lens' DeleteFleet Prelude.Text
deleteFleet_fleetArn = Lens.lens (\DeleteFleet' {fleetArn} -> fleetArn) (\s@DeleteFleet' {} a -> s {fleetArn = a} :: DeleteFleet)

instance Core.AWSRequest DeleteFleet where
  type AWSResponse DeleteFleet = DeleteFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFleet where
  hashWithSalt _salt DeleteFleet' {..} =
    _salt `Prelude.hashWithSalt` fleetArn

instance Prelude.NFData DeleteFleet where
  rnf DeleteFleet' {..} = Prelude.rnf fleetArn

instance Core.ToHeaders DeleteFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetArn" Core..= fleetArn)]
      )

instance Core.ToPath DeleteFleet where
  toPath = Prelude.const "/deleteFleet"

instance Core.ToQuery DeleteFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFleetResponse_httpStatus' - The response's http status code.
newDeleteFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFleetResponse
newDeleteFleetResponse pHttpStatus_ =
  DeleteFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFleetResponse_httpStatus :: Lens.Lens' DeleteFleetResponse Prelude.Int
deleteFleetResponse_httpStatus = Lens.lens (\DeleteFleetResponse' {httpStatus} -> httpStatus) (\s@DeleteFleetResponse' {} a -> s {httpStatus = a} :: DeleteFleetResponse)

instance Prelude.NFData DeleteFleetResponse where
  rnf DeleteFleetResponse' {..} = Prelude.rnf httpStatus
