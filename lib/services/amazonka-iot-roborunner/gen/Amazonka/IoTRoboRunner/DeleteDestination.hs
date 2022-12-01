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
-- Module      : Amazonka.IoTRoboRunner.DeleteDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to delete a destination
module Amazonka.IoTRoboRunner.DeleteDestination
  ( -- * Creating a Request
    DeleteDestination (..),
    newDeleteDestination,

    -- * Request Lenses
    deleteDestination_id,

    -- * Destructuring the Response
    DeleteDestinationResponse (..),
    newDeleteDestinationResponse,

    -- * Response Lenses
    deleteDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDestination' smart constructor.
data DeleteDestination = DeleteDestination'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteDestination_id' - Undocumented member.
newDeleteDestination ::
  -- | 'id'
  Prelude.Text ->
  DeleteDestination
newDeleteDestination pId_ =
  DeleteDestination' {id = pId_}

-- | Undocumented member.
deleteDestination_id :: Lens.Lens' DeleteDestination Prelude.Text
deleteDestination_id = Lens.lens (\DeleteDestination' {id} -> id) (\s@DeleteDestination' {} a -> s {id = a} :: DeleteDestination)

instance Core.AWSRequest DeleteDestination where
  type
    AWSResponse DeleteDestination =
      DeleteDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDestination where
  hashWithSalt _salt DeleteDestination' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteDestination where
  rnf DeleteDestination' {..} = Prelude.rnf id

instance Core.ToHeaders DeleteDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDestination where
  toJSON DeleteDestination' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath DeleteDestination where
  toPath = Prelude.const "/deleteDestination"

instance Core.ToQuery DeleteDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDestinationResponse' smart constructor.
data DeleteDestinationResponse = DeleteDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDestinationResponse_httpStatus' - The response's http status code.
newDeleteDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDestinationResponse
newDeleteDestinationResponse pHttpStatus_ =
  DeleteDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDestinationResponse_httpStatus :: Lens.Lens' DeleteDestinationResponse Prelude.Int
deleteDestinationResponse_httpStatus = Lens.lens (\DeleteDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteDestinationResponse' {} a -> s {httpStatus = a} :: DeleteDestinationResponse)

instance Prelude.NFData DeleteDestinationResponse where
  rnf DeleteDestinationResponse' {..} =
    Prelude.rnf httpStatus
