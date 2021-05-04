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
-- Module      : Network.AWS.Glue.UpdateTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a trigger definition.
module Network.AWS.Glue.UpdateTrigger
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTrigger' smart constructor.
data UpdateTrigger = UpdateTrigger'
  { -- | The name of the trigger to update.
    name :: Prelude.Text,
    -- | The new values with which to update the trigger.
    triggerUpdate :: TriggerUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateTrigger where
  type Rs UpdateTrigger = UpdateTriggerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTriggerResponse'
            Prelude.<$> (x Prelude..?> "Trigger")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrigger

instance Prelude.NFData UpdateTrigger

instance Prelude.ToHeaders UpdateTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateTrigger" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTrigger where
  toJSON UpdateTrigger' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ("TriggerUpdate" Prelude..= triggerUpdate)
          ]
      )

instance Prelude.ToPath UpdateTrigger where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTriggerResponse' smart constructor.
data UpdateTriggerResponse = UpdateTriggerResponse'
  { -- | The resulting trigger definition.
    trigger :: Prelude.Maybe Trigger,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateTriggerResponse
