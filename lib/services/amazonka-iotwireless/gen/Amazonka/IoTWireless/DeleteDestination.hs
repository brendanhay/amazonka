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
-- Module      : Amazonka.IoTWireless.DeleteDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a destination.
module Amazonka.IoTWireless.DeleteDestination
  ( -- * Creating a Request
    DeleteDestination (..),
    newDeleteDestination,

    -- * Request Lenses
    deleteDestination_name,

    -- * Destructuring the Response
    DeleteDestinationResponse (..),
    newDeleteDestinationResponse,

    -- * Response Lenses
    deleteDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDestination' smart constructor.
data DeleteDestination = DeleteDestination'
  { -- | The name of the resource to delete.
    name :: Prelude.Text
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
-- 'name', 'deleteDestination_name' - The name of the resource to delete.
newDeleteDestination ::
  -- | 'name'
  Prelude.Text ->
  DeleteDestination
newDeleteDestination pName_ =
  DeleteDestination' {name = pName_}

-- | The name of the resource to delete.
deleteDestination_name :: Lens.Lens' DeleteDestination Prelude.Text
deleteDestination_name = Lens.lens (\DeleteDestination' {name} -> name) (\s@DeleteDestination' {} a -> s {name = a} :: DeleteDestination)

instance Core.AWSRequest DeleteDestination where
  type
    AWSResponse DeleteDestination =
      DeleteDestinationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDestination where
  hashWithSalt _salt DeleteDestination' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteDestination where
  rnf DeleteDestination' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteDestination where
  toPath DeleteDestination' {..} =
    Prelude.mconcat ["/destinations/", Core.toBS name]

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
