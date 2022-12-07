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
-- Module      : Amazonka.Glue.DeleteTrigger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified trigger. If the trigger is not found, no exception
-- is thrown.
module Amazonka.Glue.DeleteTrigger
  ( -- * Creating a Request
    DeleteTrigger (..),
    newDeleteTrigger,

    -- * Request Lenses
    deleteTrigger_name,

    -- * Destructuring the Response
    DeleteTriggerResponse (..),
    newDeleteTriggerResponse,

    -- * Response Lenses
    deleteTriggerResponse_name,
    deleteTriggerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTrigger' smart constructor.
data DeleteTrigger = DeleteTrigger'
  { -- | The name of the trigger to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteTrigger_name' - The name of the trigger to delete.
newDeleteTrigger ::
  -- | 'name'
  Prelude.Text ->
  DeleteTrigger
newDeleteTrigger pName_ =
  DeleteTrigger' {name = pName_}

-- | The name of the trigger to delete.
deleteTrigger_name :: Lens.Lens' DeleteTrigger Prelude.Text
deleteTrigger_name = Lens.lens (\DeleteTrigger' {name} -> name) (\s@DeleteTrigger' {} a -> s {name = a} :: DeleteTrigger)

instance Core.AWSRequest DeleteTrigger where
  type
    AWSResponse DeleteTrigger =
      DeleteTriggerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTriggerResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrigger where
  hashWithSalt _salt DeleteTrigger' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteTrigger where
  rnf DeleteTrigger' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.DeleteTrigger" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTrigger where
  toJSON DeleteTrigger' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteTrigger where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTriggerResponse' smart constructor.
data DeleteTriggerResponse = DeleteTriggerResponse'
  { -- | The name of the trigger that was deleted.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTriggerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteTriggerResponse_name' - The name of the trigger that was deleted.
--
-- 'httpStatus', 'deleteTriggerResponse_httpStatus' - The response's http status code.
newDeleteTriggerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTriggerResponse
newDeleteTriggerResponse pHttpStatus_ =
  DeleteTriggerResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the trigger that was deleted.
deleteTriggerResponse_name :: Lens.Lens' DeleteTriggerResponse (Prelude.Maybe Prelude.Text)
deleteTriggerResponse_name = Lens.lens (\DeleteTriggerResponse' {name} -> name) (\s@DeleteTriggerResponse' {} a -> s {name = a} :: DeleteTriggerResponse)

-- | The response's http status code.
deleteTriggerResponse_httpStatus :: Lens.Lens' DeleteTriggerResponse Prelude.Int
deleteTriggerResponse_httpStatus = Lens.lens (\DeleteTriggerResponse' {httpStatus} -> httpStatus) (\s@DeleteTriggerResponse' {} a -> s {httpStatus = a} :: DeleteTriggerResponse)

instance Prelude.NFData DeleteTriggerResponse where
  rnf DeleteTriggerResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
