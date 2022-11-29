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
-- Module      : Amazonka.IoTWireless.StartFuotaTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a FUOTA task.
module Amazonka.IoTWireless.StartFuotaTask
  ( -- * Creating a Request
    StartFuotaTask (..),
    newStartFuotaTask,

    -- * Request Lenses
    startFuotaTask_loRaWAN,
    startFuotaTask_id,

    -- * Destructuring the Response
    StartFuotaTaskResponse (..),
    newStartFuotaTaskResponse,

    -- * Response Lenses
    startFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFuotaTask' smart constructor.
data StartFuotaTask = StartFuotaTask'
  { loRaWAN :: Prelude.Maybe LoRaWANStartFuotaTask,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRaWAN', 'startFuotaTask_loRaWAN' - Undocumented member.
--
-- 'id', 'startFuotaTask_id' - Undocumented member.
newStartFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  StartFuotaTask
newStartFuotaTask pId_ =
  StartFuotaTask'
    { loRaWAN = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
startFuotaTask_loRaWAN :: Lens.Lens' StartFuotaTask (Prelude.Maybe LoRaWANStartFuotaTask)
startFuotaTask_loRaWAN = Lens.lens (\StartFuotaTask' {loRaWAN} -> loRaWAN) (\s@StartFuotaTask' {} a -> s {loRaWAN = a} :: StartFuotaTask)

-- | Undocumented member.
startFuotaTask_id :: Lens.Lens' StartFuotaTask Prelude.Text
startFuotaTask_id = Lens.lens (\StartFuotaTask' {id} -> id) (\s@StartFuotaTask' {} a -> s {id = a} :: StartFuotaTask)

instance Core.AWSRequest StartFuotaTask where
  type
    AWSResponse StartFuotaTask =
      StartFuotaTaskResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartFuotaTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFuotaTask where
  hashWithSalt _salt StartFuotaTask' {..} =
    _salt `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` id

instance Prelude.NFData StartFuotaTask where
  rnf StartFuotaTask' {..} =
    Prelude.rnf loRaWAN `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders StartFuotaTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StartFuotaTask where
  toJSON StartFuotaTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [("LoRaWAN" Core..=) Prelude.<$> loRaWAN]
      )

instance Core.ToPath StartFuotaTask where
  toPath StartFuotaTask' {..} =
    Prelude.mconcat ["/fuota-tasks/", Core.toBS id]

instance Core.ToQuery StartFuotaTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFuotaTaskResponse' smart constructor.
data StartFuotaTaskResponse = StartFuotaTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startFuotaTaskResponse_httpStatus' - The response's http status code.
newStartFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFuotaTaskResponse
newStartFuotaTaskResponse pHttpStatus_ =
  StartFuotaTaskResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startFuotaTaskResponse_httpStatus :: Lens.Lens' StartFuotaTaskResponse Prelude.Int
startFuotaTaskResponse_httpStatus = Lens.lens (\StartFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@StartFuotaTaskResponse' {} a -> s {httpStatus = a} :: StartFuotaTaskResponse)

instance Prelude.NFData StartFuotaTaskResponse where
  rnf StartFuotaTaskResponse' {..} =
    Prelude.rnf httpStatus
