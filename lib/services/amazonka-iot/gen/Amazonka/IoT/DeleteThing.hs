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
-- Module      : Amazonka.IoT.DeleteThing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing. Returns successfully with no error if the
-- deletion is successful or you specify a thing that doesn\'t exist.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteThing>
-- action.
module Amazonka.IoT.DeleteThing
  ( -- * Creating a Request
    DeleteThing (..),
    newDeleteThing,

    -- * Request Lenses
    deleteThing_expectedVersion,
    deleteThing_thingName,

    -- * Destructuring the Response
    DeleteThingResponse (..),
    newDeleteThingResponse,

    -- * Response Lenses
    deleteThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeleteThing operation.
--
-- /See:/ 'newDeleteThing' smart constructor.
data DeleteThing = DeleteThing'
  { -- | The expected version of the thing record in the registry. If the version
    -- of the record in the registry does not match the expected version
    -- specified in the request, the @DeleteThing@ request is rejected with a
    -- @VersionConflictException@.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the thing to delete.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'deleteThing_expectedVersion' - The expected version of the thing record in the registry. If the version
-- of the record in the registry does not match the expected version
-- specified in the request, the @DeleteThing@ request is rejected with a
-- @VersionConflictException@.
--
-- 'thingName', 'deleteThing_thingName' - The name of the thing to delete.
newDeleteThing ::
  -- | 'thingName'
  Prelude.Text ->
  DeleteThing
newDeleteThing pThingName_ =
  DeleteThing'
    { expectedVersion = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The expected version of the thing record in the registry. If the version
-- of the record in the registry does not match the expected version
-- specified in the request, the @DeleteThing@ request is rejected with a
-- @VersionConflictException@.
deleteThing_expectedVersion :: Lens.Lens' DeleteThing (Prelude.Maybe Prelude.Integer)
deleteThing_expectedVersion = Lens.lens (\DeleteThing' {expectedVersion} -> expectedVersion) (\s@DeleteThing' {} a -> s {expectedVersion = a} :: DeleteThing)

-- | The name of the thing to delete.
deleteThing_thingName :: Lens.Lens' DeleteThing Prelude.Text
deleteThing_thingName = Lens.lens (\DeleteThing' {thingName} -> thingName) (\s@DeleteThing' {} a -> s {thingName = a} :: DeleteThing)

instance Core.AWSRequest DeleteThing where
  type AWSResponse DeleteThing = DeleteThingResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteThing where
  hashWithSalt _salt DeleteThing' {..} =
    _salt `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData DeleteThing where
  rnf DeleteThing' {..} =
    Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders DeleteThing where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteThing where
  toPath DeleteThing' {..} =
    Prelude.mconcat ["/things/", Data.toBS thingName]

instance Data.ToQuery DeleteThing where
  toQuery DeleteThing' {..} =
    Prelude.mconcat
      ["expectedVersion" Data.=: expectedVersion]

-- | The output of the DeleteThing operation.
--
-- /See:/ 'newDeleteThingResponse' smart constructor.
data DeleteThingResponse = DeleteThingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteThingResponse_httpStatus' - The response's http status code.
newDeleteThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteThingResponse
newDeleteThingResponse pHttpStatus_ =
  DeleteThingResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteThingResponse_httpStatus :: Lens.Lens' DeleteThingResponse Prelude.Int
deleteThingResponse_httpStatus = Lens.lens (\DeleteThingResponse' {httpStatus} -> httpStatus) (\s@DeleteThingResponse' {} a -> s {httpStatus = a} :: DeleteThingResponse)

instance Prelude.NFData DeleteThingResponse where
  rnf DeleteThingResponse' {..} = Prelude.rnf httpStatus
