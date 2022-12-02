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
-- Module      : Amazonka.SageMakerA2IRuntime.StopHumanLoop
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified human loop.
module Amazonka.SageMakerA2IRuntime.StopHumanLoop
  ( -- * Creating a Request
    StopHumanLoop (..),
    newStopHumanLoop,

    -- * Request Lenses
    stopHumanLoop_humanLoopName,

    -- * Destructuring the Response
    StopHumanLoopResponse (..),
    newStopHumanLoopResponse,

    -- * Response Lenses
    stopHumanLoopResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerA2IRuntime.Types

-- | /See:/ 'newStopHumanLoop' smart constructor.
data StopHumanLoop = StopHumanLoop'
  { -- | The name of the human loop that you want to stop.
    humanLoopName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopHumanLoop' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopName', 'stopHumanLoop_humanLoopName' - The name of the human loop that you want to stop.
newStopHumanLoop ::
  -- | 'humanLoopName'
  Prelude.Text ->
  StopHumanLoop
newStopHumanLoop pHumanLoopName_ =
  StopHumanLoop' {humanLoopName = pHumanLoopName_}

-- | The name of the human loop that you want to stop.
stopHumanLoop_humanLoopName :: Lens.Lens' StopHumanLoop Prelude.Text
stopHumanLoop_humanLoopName = Lens.lens (\StopHumanLoop' {humanLoopName} -> humanLoopName) (\s@StopHumanLoop' {} a -> s {humanLoopName = a} :: StopHumanLoop)

instance Core.AWSRequest StopHumanLoop where
  type
    AWSResponse StopHumanLoop =
      StopHumanLoopResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopHumanLoopResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopHumanLoop where
  hashWithSalt _salt StopHumanLoop' {..} =
    _salt `Prelude.hashWithSalt` humanLoopName

instance Prelude.NFData StopHumanLoop where
  rnf StopHumanLoop' {..} = Prelude.rnf humanLoopName

instance Data.ToHeaders StopHumanLoop where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StopHumanLoop where
  toJSON StopHumanLoop' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HumanLoopName" Data..= humanLoopName)
          ]
      )

instance Data.ToPath StopHumanLoop where
  toPath = Prelude.const "/human-loops/stop"

instance Data.ToQuery StopHumanLoop where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopHumanLoopResponse' smart constructor.
data StopHumanLoopResponse = StopHumanLoopResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopHumanLoopResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopHumanLoopResponse_httpStatus' - The response's http status code.
newStopHumanLoopResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopHumanLoopResponse
newStopHumanLoopResponse pHttpStatus_ =
  StopHumanLoopResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopHumanLoopResponse_httpStatus :: Lens.Lens' StopHumanLoopResponse Prelude.Int
stopHumanLoopResponse_httpStatus = Lens.lens (\StopHumanLoopResponse' {httpStatus} -> httpStatus) (\s@StopHumanLoopResponse' {} a -> s {httpStatus = a} :: StopHumanLoopResponse)

instance Prelude.NFData StopHumanLoopResponse where
  rnf StopHumanLoopResponse' {..} =
    Prelude.rnf httpStatus
