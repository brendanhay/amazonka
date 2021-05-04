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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartTrigger' smart constructor.
data StartTrigger = StartTrigger'
  { -- | The name of the trigger to start.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StartTrigger
newStartTrigger pName_ = StartTrigger' {name = pName_}

-- | The name of the trigger to start.
startTrigger_name :: Lens.Lens' StartTrigger Prelude.Text
startTrigger_name = Lens.lens (\StartTrigger' {name} -> name) (\s@StartTrigger' {} a -> s {name = a} :: StartTrigger)

instance Prelude.AWSRequest StartTrigger where
  type Rs StartTrigger = StartTriggerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTriggerResponse'
            Prelude.<$> (x Prelude..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTrigger

instance Prelude.NFData StartTrigger

instance Prelude.ToHeaders StartTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.StartTrigger" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartTrigger where
  toJSON StartTrigger' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StartTrigger where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTriggerResponse' smart constructor.
data StartTriggerResponse = StartTriggerResponse'
  { -- | The name of the trigger that was started.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StartTriggerResponse
newStartTriggerResponse pHttpStatus_ =
  StartTriggerResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the trigger that was started.
startTriggerResponse_name :: Lens.Lens' StartTriggerResponse (Prelude.Maybe Prelude.Text)
startTriggerResponse_name = Lens.lens (\StartTriggerResponse' {name} -> name) (\s@StartTriggerResponse' {} a -> s {name = a} :: StartTriggerResponse)

-- | The response's http status code.
startTriggerResponse_httpStatus :: Lens.Lens' StartTriggerResponse Prelude.Int
startTriggerResponse_httpStatus = Lens.lens (\StartTriggerResponse' {httpStatus} -> httpStatus) (\s@StartTriggerResponse' {} a -> s {httpStatus = a} :: StartTriggerResponse)

instance Prelude.NFData StartTriggerResponse
