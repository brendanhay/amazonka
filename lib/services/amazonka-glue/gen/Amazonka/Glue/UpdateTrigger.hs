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
-- Module      : Amazonka.Glue.UpdateTrigger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a trigger definition.
module Amazonka.Glue.UpdateTrigger
  ( -- * Creating a Request
    UpdateTrigger (..),
    newUpdateTrigger,

    -- * Request Lenses
    updateTrigger_name,
    updateTrigger_triggerUpdate,

    -- * Destructuring the Response
    UpdateTriggerResponse (..),
    newUpdateTriggerResponse,

    -- * Response Lenses
    updateTriggerResponse_trigger,
    updateTriggerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTrigger' smart constructor.
data UpdateTrigger = UpdateTrigger'
  { -- | The name of the trigger to update.
    name :: Prelude.Text,
    -- | The new values with which to update the trigger.
    triggerUpdate :: TriggerUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateTrigger_name' - The name of the trigger to update.
--
-- 'triggerUpdate', 'updateTrigger_triggerUpdate' - The new values with which to update the trigger.
newUpdateTrigger ::
  -- | 'name'
  Prelude.Text ->
  -- | 'triggerUpdate'
  TriggerUpdate ->
  UpdateTrigger
newUpdateTrigger pName_ pTriggerUpdate_ =
  UpdateTrigger'
    { name = pName_,
      triggerUpdate = pTriggerUpdate_
    }

-- | The name of the trigger to update.
updateTrigger_name :: Lens.Lens' UpdateTrigger Prelude.Text
updateTrigger_name = Lens.lens (\UpdateTrigger' {name} -> name) (\s@UpdateTrigger' {} a -> s {name = a} :: UpdateTrigger)

-- | The new values with which to update the trigger.
updateTrigger_triggerUpdate :: Lens.Lens' UpdateTrigger TriggerUpdate
updateTrigger_triggerUpdate = Lens.lens (\UpdateTrigger' {triggerUpdate} -> triggerUpdate) (\s@UpdateTrigger' {} a -> s {triggerUpdate = a} :: UpdateTrigger)

instance Core.AWSRequest UpdateTrigger where
  type
    AWSResponse UpdateTrigger =
      UpdateTriggerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTriggerResponse'
            Prelude.<$> (x Data..?> "Trigger")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrigger where
  hashWithSalt _salt UpdateTrigger' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` triggerUpdate

instance Prelude.NFData UpdateTrigger where
  rnf UpdateTrigger' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf triggerUpdate

instance Data.ToHeaders UpdateTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateTrigger" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTrigger where
  toJSON UpdateTrigger' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("TriggerUpdate" Data..= triggerUpdate)
          ]
      )

instance Data.ToPath UpdateTrigger where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTriggerResponse' smart constructor.
data UpdateTriggerResponse = UpdateTriggerResponse'
  { -- | The resulting trigger definition.
    trigger :: Prelude.Maybe Trigger,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTriggerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trigger', 'updateTriggerResponse_trigger' - The resulting trigger definition.
--
-- 'httpStatus', 'updateTriggerResponse_httpStatus' - The response's http status code.
newUpdateTriggerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTriggerResponse
newUpdateTriggerResponse pHttpStatus_ =
  UpdateTriggerResponse'
    { trigger = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resulting trigger definition.
updateTriggerResponse_trigger :: Lens.Lens' UpdateTriggerResponse (Prelude.Maybe Trigger)
updateTriggerResponse_trigger = Lens.lens (\UpdateTriggerResponse' {trigger} -> trigger) (\s@UpdateTriggerResponse' {} a -> s {trigger = a} :: UpdateTriggerResponse)

-- | The response's http status code.
updateTriggerResponse_httpStatus :: Lens.Lens' UpdateTriggerResponse Prelude.Int
updateTriggerResponse_httpStatus = Lens.lens (\UpdateTriggerResponse' {httpStatus} -> httpStatus) (\s@UpdateTriggerResponse' {} a -> s {httpStatus = a} :: UpdateTriggerResponse)

instance Prelude.NFData UpdateTriggerResponse where
  rnf UpdateTriggerResponse' {..} =
    Prelude.rnf trigger
      `Prelude.seq` Prelude.rnf httpStatus
