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
-- Module      : Amazonka.CloudWatchEvents.DeleteApiDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API destination.
module Amazonka.CloudWatchEvents.DeleteApiDestination
  ( -- * Creating a Request
    DeleteApiDestination (..),
    newDeleteApiDestination,

    -- * Request Lenses
    deleteApiDestination_name,

    -- * Destructuring the Response
    DeleteApiDestinationResponse (..),
    newDeleteApiDestinationResponse,

    -- * Response Lenses
    deleteApiDestinationResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApiDestination' smart constructor.
data DeleteApiDestination = DeleteApiDestination'
  { -- | The name of the destination to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteApiDestination_name' - The name of the destination to delete.
newDeleteApiDestination ::
  -- | 'name'
  Prelude.Text ->
  DeleteApiDestination
newDeleteApiDestination pName_ =
  DeleteApiDestination' {name = pName_}

-- | The name of the destination to delete.
deleteApiDestination_name :: Lens.Lens' DeleteApiDestination Prelude.Text
deleteApiDestination_name = Lens.lens (\DeleteApiDestination' {name} -> name) (\s@DeleteApiDestination' {} a -> s {name = a} :: DeleteApiDestination)

instance Core.AWSRequest DeleteApiDestination where
  type
    AWSResponse DeleteApiDestination =
      DeleteApiDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApiDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApiDestination where
  hashWithSalt _salt DeleteApiDestination' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteApiDestination where
  rnf DeleteApiDestination' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteApiDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.DeleteApiDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApiDestination where
  toJSON DeleteApiDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteApiDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApiDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApiDestinationResponse' smart constructor.
data DeleteApiDestinationResponse = DeleteApiDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApiDestinationResponse_httpStatus' - The response's http status code.
newDeleteApiDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApiDestinationResponse
newDeleteApiDestinationResponse pHttpStatus_ =
  DeleteApiDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApiDestinationResponse_httpStatus :: Lens.Lens' DeleteApiDestinationResponse Prelude.Int
deleteApiDestinationResponse_httpStatus = Lens.lens (\DeleteApiDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteApiDestinationResponse' {} a -> s {httpStatus = a} :: DeleteApiDestinationResponse)

instance Prelude.NFData DeleteApiDestinationResponse where
  rnf DeleteApiDestinationResponse' {..} =
    Prelude.rnf httpStatus
