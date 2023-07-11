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
-- Module      : Amazonka.IoTWireless.ResetAllResourceLogLevels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the log-level overrides for all resources; both wireless devices
-- and wireless gateways.
module Amazonka.IoTWireless.ResetAllResourceLogLevels
  ( -- * Creating a Request
    ResetAllResourceLogLevels (..),
    newResetAllResourceLogLevels,

    -- * Destructuring the Response
    ResetAllResourceLogLevelsResponse (..),
    newResetAllResourceLogLevelsResponse,

    -- * Response Lenses
    resetAllResourceLogLevelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetAllResourceLogLevels' smart constructor.
data ResetAllResourceLogLevels = ResetAllResourceLogLevels'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetAllResourceLogLevels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResetAllResourceLogLevels ::
  ResetAllResourceLogLevels
newResetAllResourceLogLevels =
  ResetAllResourceLogLevels'

instance Core.AWSRequest ResetAllResourceLogLevels where
  type
    AWSResponse ResetAllResourceLogLevels =
      ResetAllResourceLogLevelsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResetAllResourceLogLevelsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetAllResourceLogLevels where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ResetAllResourceLogLevels where
  rnf _ = ()

instance Data.ToHeaders ResetAllResourceLogLevels where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetAllResourceLogLevels where
  toPath = Prelude.const "/log-levels"

instance Data.ToQuery ResetAllResourceLogLevels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetAllResourceLogLevelsResponse' smart constructor.
data ResetAllResourceLogLevelsResponse = ResetAllResourceLogLevelsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetAllResourceLogLevelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resetAllResourceLogLevelsResponse_httpStatus' - The response's http status code.
newResetAllResourceLogLevelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetAllResourceLogLevelsResponse
newResetAllResourceLogLevelsResponse pHttpStatus_ =
  ResetAllResourceLogLevelsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
resetAllResourceLogLevelsResponse_httpStatus :: Lens.Lens' ResetAllResourceLogLevelsResponse Prelude.Int
resetAllResourceLogLevelsResponse_httpStatus = Lens.lens (\ResetAllResourceLogLevelsResponse' {httpStatus} -> httpStatus) (\s@ResetAllResourceLogLevelsResponse' {} a -> s {httpStatus = a} :: ResetAllResourceLogLevelsResponse)

instance
  Prelude.NFData
    ResetAllResourceLogLevelsResponse
  where
  rnf ResetAllResourceLogLevelsResponse' {..} =
    Prelude.rnf httpStatus
