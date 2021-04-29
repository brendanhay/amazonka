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
-- Module      : Network.AWS.CloudWatchEvents.DeleteApiDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API destination.
module Network.AWS.CloudWatchEvents.DeleteApiDestination
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

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApiDestination' smart constructor.
data DeleteApiDestination = DeleteApiDestination'
  { -- | The name of the destination to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteApiDestination where
  type
    Rs DeleteApiDestination =
      DeleteApiDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApiDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApiDestination

instance Prelude.NFData DeleteApiDestination

instance Prelude.ToHeaders DeleteApiDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSEvents.DeleteApiDestination" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteApiDestination where
  toJSON DeleteApiDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteApiDestination where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteApiDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApiDestinationResponse' smart constructor.
data DeleteApiDestinationResponse = DeleteApiDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteApiDestinationResponse
