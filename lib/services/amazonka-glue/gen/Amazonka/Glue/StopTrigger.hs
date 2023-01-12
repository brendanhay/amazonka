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
-- Module      : Amazonka.Glue.StopTrigger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified trigger.
module Amazonka.Glue.StopTrigger
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopTrigger' smart constructor.
data StopTrigger = StopTrigger'
  { -- | The name of the trigger to stop.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopTrigger
newStopTrigger pName_ = StopTrigger' {name = pName_}

-- | The name of the trigger to stop.
stopTrigger_name :: Lens.Lens' StopTrigger Prelude.Text
stopTrigger_name = Lens.lens (\StopTrigger' {name} -> name) (\s@StopTrigger' {} a -> s {name = a} :: StopTrigger)

instance Core.AWSRequest StopTrigger where
  type AWSResponse StopTrigger = StopTriggerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopTriggerResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopTrigger where
  hashWithSalt _salt StopTrigger' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StopTrigger where
  rnf StopTrigger' {..} = Prelude.rnf name

instance Data.ToHeaders StopTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.StopTrigger" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopTrigger where
  toJSON StopTrigger' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath StopTrigger where
  toPath = Prelude.const "/"

instance Data.ToQuery StopTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTriggerResponse' smart constructor.
data StopTriggerResponse = StopTriggerResponse'
  { -- | The name of the trigger that was stopped.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StopTriggerResponse
newStopTriggerResponse pHttpStatus_ =
  StopTriggerResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the trigger that was stopped.
stopTriggerResponse_name :: Lens.Lens' StopTriggerResponse (Prelude.Maybe Prelude.Text)
stopTriggerResponse_name = Lens.lens (\StopTriggerResponse' {name} -> name) (\s@StopTriggerResponse' {} a -> s {name = a} :: StopTriggerResponse)

-- | The response's http status code.
stopTriggerResponse_httpStatus :: Lens.Lens' StopTriggerResponse Prelude.Int
stopTriggerResponse_httpStatus = Lens.lens (\StopTriggerResponse' {httpStatus} -> httpStatus) (\s@StopTriggerResponse' {} a -> s {httpStatus = a} :: StopTriggerResponse)

instance Prelude.NFData StopTriggerResponse where
  rnf StopTriggerResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
