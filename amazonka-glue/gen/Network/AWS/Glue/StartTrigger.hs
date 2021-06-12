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
-- Module      : Network.AWS.Glue.StartTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing trigger. See
-- <https://docs.aws.amazon.com/glue/latest/dg/trigger-job.html Triggering Jobs>
-- for information about how different types of trigger are started.
module Network.AWS.Glue.StartTrigger
  ( -- * Creating a Request
    StartTrigger (..),
    newStartTrigger,

    -- * Request Lenses
    startTrigger_name,

    -- * Destructuring the Response
    StartTriggerResponse (..),
    newStartTriggerResponse,

    -- * Response Lenses
    startTriggerResponse_name,
    startTriggerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartTrigger' smart constructor.
data StartTrigger = StartTrigger'
  { -- | The name of the trigger to start.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startTrigger_name' - The name of the trigger to start.
newStartTrigger ::
  -- | 'name'
  Core.Text ->
  StartTrigger
newStartTrigger pName_ = StartTrigger' {name = pName_}

-- | The name of the trigger to start.
startTrigger_name :: Lens.Lens' StartTrigger Core.Text
startTrigger_name = Lens.lens (\StartTrigger' {name} -> name) (\s@StartTrigger' {} a -> s {name = a} :: StartTrigger)

instance Core.AWSRequest StartTrigger where
  type AWSResponse StartTrigger = StartTriggerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTriggerResponse'
            Core.<$> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartTrigger

instance Core.NFData StartTrigger

instance Core.ToHeaders StartTrigger where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.StartTrigger" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartTrigger where
  toJSON StartTrigger' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StartTrigger where
  toPath = Core.const "/"

instance Core.ToQuery StartTrigger where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartTriggerResponse' smart constructor.
data StartTriggerResponse = StartTriggerResponse'
  { -- | The name of the trigger that was started.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartTriggerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startTriggerResponse_name' - The name of the trigger that was started.
--
-- 'httpStatus', 'startTriggerResponse_httpStatus' - The response's http status code.
newStartTriggerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartTriggerResponse
newStartTriggerResponse pHttpStatus_ =
  StartTriggerResponse'
    { name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the trigger that was started.
startTriggerResponse_name :: Lens.Lens' StartTriggerResponse (Core.Maybe Core.Text)
startTriggerResponse_name = Lens.lens (\StartTriggerResponse' {name} -> name) (\s@StartTriggerResponse' {} a -> s {name = a} :: StartTriggerResponse)

-- | The response's http status code.
startTriggerResponse_httpStatus :: Lens.Lens' StartTriggerResponse Core.Int
startTriggerResponse_httpStatus = Lens.lens (\StartTriggerResponse' {httpStatus} -> httpStatus) (\s@StartTriggerResponse' {} a -> s {httpStatus = a} :: StartTriggerResponse)

instance Core.NFData StartTriggerResponse
