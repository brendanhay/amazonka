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
-- Module      : Network.AWS.Glue.StopTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified trigger.
module Network.AWS.Glue.StopTrigger
  ( -- * Creating a Request
    StopTrigger (..),
    newStopTrigger,

    -- * Request Lenses
    stopTrigger_name,

    -- * Destructuring the Response
    StopTriggerResponse (..),
    newStopTriggerResponse,

    -- * Response Lenses
    stopTriggerResponse_name,
    stopTriggerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopTrigger' smart constructor.
data StopTrigger = StopTrigger'
  { -- | The name of the trigger to stop.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopTrigger_name' - The name of the trigger to stop.
newStopTrigger ::
  -- | 'name'
  Core.Text ->
  StopTrigger
newStopTrigger pName_ = StopTrigger' {name = pName_}

-- | The name of the trigger to stop.
stopTrigger_name :: Lens.Lens' StopTrigger Core.Text
stopTrigger_name = Lens.lens (\StopTrigger' {name} -> name) (\s@StopTrigger' {} a -> s {name = a} :: StopTrigger)

instance Core.AWSRequest StopTrigger where
  type AWSResponse StopTrigger = StopTriggerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopTriggerResponse'
            Core.<$> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopTrigger

instance Core.NFData StopTrigger

instance Core.ToHeaders StopTrigger where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.StopTrigger" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopTrigger where
  toJSON StopTrigger' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StopTrigger where
  toPath = Core.const "/"

instance Core.ToQuery StopTrigger where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopTriggerResponse' smart constructor.
data StopTriggerResponse = StopTriggerResponse'
  { -- | The name of the trigger that was stopped.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopTriggerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopTriggerResponse_name' - The name of the trigger that was stopped.
--
-- 'httpStatus', 'stopTriggerResponse_httpStatus' - The response's http status code.
newStopTriggerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopTriggerResponse
newStopTriggerResponse pHttpStatus_ =
  StopTriggerResponse'
    { name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the trigger that was stopped.
stopTriggerResponse_name :: Lens.Lens' StopTriggerResponse (Core.Maybe Core.Text)
stopTriggerResponse_name = Lens.lens (\StopTriggerResponse' {name} -> name) (\s@StopTriggerResponse' {} a -> s {name = a} :: StopTriggerResponse)

-- | The response's http status code.
stopTriggerResponse_httpStatus :: Lens.Lens' StopTriggerResponse Core.Int
stopTriggerResponse_httpStatus = Lens.lens (\StopTriggerResponse' {httpStatus} -> httpStatus) (\s@StopTriggerResponse' {} a -> s {httpStatus = a} :: StopTriggerResponse)

instance Core.NFData StopTriggerResponse
