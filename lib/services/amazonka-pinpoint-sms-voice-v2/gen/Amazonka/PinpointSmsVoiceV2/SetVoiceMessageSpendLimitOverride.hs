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
-- Module      : Amazonka.PinpointSmsVoiceV2.SetVoiceMessageSpendLimitOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an account level monthly spend limit override for sending voice
-- messages. The requested spend limit must be less than or equal to the
-- @MaxLimit@, which is set by Amazon Web Services.
module Amazonka.PinpointSmsVoiceV2.SetVoiceMessageSpendLimitOverride
  ( -- * Creating a Request
    SetVoiceMessageSpendLimitOverride (..),
    newSetVoiceMessageSpendLimitOverride,

    -- * Request Lenses
    setVoiceMessageSpendLimitOverride_monthlyLimit,

    -- * Destructuring the Response
    SetVoiceMessageSpendLimitOverrideResponse (..),
    newSetVoiceMessageSpendLimitOverrideResponse,

    -- * Response Lenses
    setVoiceMessageSpendLimitOverrideResponse_monthlyLimit,
    setVoiceMessageSpendLimitOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetVoiceMessageSpendLimitOverride' smart constructor.
data SetVoiceMessageSpendLimitOverride = SetVoiceMessageSpendLimitOverride'
  { -- | The new monthly limit to enforce on voice messages.
    monthlyLimit :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetVoiceMessageSpendLimitOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monthlyLimit', 'setVoiceMessageSpendLimitOverride_monthlyLimit' - The new monthly limit to enforce on voice messages.
newSetVoiceMessageSpendLimitOverride ::
  -- | 'monthlyLimit'
  Prelude.Natural ->
  SetVoiceMessageSpendLimitOverride
newSetVoiceMessageSpendLimitOverride pMonthlyLimit_ =
  SetVoiceMessageSpendLimitOverride'
    { monthlyLimit =
        pMonthlyLimit_
    }

-- | The new monthly limit to enforce on voice messages.
setVoiceMessageSpendLimitOverride_monthlyLimit :: Lens.Lens' SetVoiceMessageSpendLimitOverride Prelude.Natural
setVoiceMessageSpendLimitOverride_monthlyLimit = Lens.lens (\SetVoiceMessageSpendLimitOverride' {monthlyLimit} -> monthlyLimit) (\s@SetVoiceMessageSpendLimitOverride' {} a -> s {monthlyLimit = a} :: SetVoiceMessageSpendLimitOverride)

instance
  Core.AWSRequest
    SetVoiceMessageSpendLimitOverride
  where
  type
    AWSResponse SetVoiceMessageSpendLimitOverride =
      SetVoiceMessageSpendLimitOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetVoiceMessageSpendLimitOverrideResponse'
            Prelude.<$> (x Data..?> "MonthlyLimit")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetVoiceMessageSpendLimitOverride
  where
  hashWithSalt
    _salt
    SetVoiceMessageSpendLimitOverride' {..} =
      _salt `Prelude.hashWithSalt` monthlyLimit

instance
  Prelude.NFData
    SetVoiceMessageSpendLimitOverride
  where
  rnf SetVoiceMessageSpendLimitOverride' {..} =
    Prelude.rnf monthlyLimit

instance
  Data.ToHeaders
    SetVoiceMessageSpendLimitOverride
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.SetVoiceMessageSpendLimitOverride" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    SetVoiceMessageSpendLimitOverride
  where
  toJSON SetVoiceMessageSpendLimitOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MonthlyLimit" Data..= monthlyLimit)]
      )

instance
  Data.ToPath
    SetVoiceMessageSpendLimitOverride
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    SetVoiceMessageSpendLimitOverride
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetVoiceMessageSpendLimitOverrideResponse' smart constructor.
data SetVoiceMessageSpendLimitOverrideResponse = SetVoiceMessageSpendLimitOverrideResponse'
  { -- | The current monthly limit to enforce on sending voice messages.
    monthlyLimit :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetVoiceMessageSpendLimitOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monthlyLimit', 'setVoiceMessageSpendLimitOverrideResponse_monthlyLimit' - The current monthly limit to enforce on sending voice messages.
--
-- 'httpStatus', 'setVoiceMessageSpendLimitOverrideResponse_httpStatus' - The response's http status code.
newSetVoiceMessageSpendLimitOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetVoiceMessageSpendLimitOverrideResponse
newSetVoiceMessageSpendLimitOverrideResponse
  pHttpStatus_ =
    SetVoiceMessageSpendLimitOverrideResponse'
      { monthlyLimit =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current monthly limit to enforce on sending voice messages.
setVoiceMessageSpendLimitOverrideResponse_monthlyLimit :: Lens.Lens' SetVoiceMessageSpendLimitOverrideResponse (Prelude.Maybe Prelude.Natural)
setVoiceMessageSpendLimitOverrideResponse_monthlyLimit = Lens.lens (\SetVoiceMessageSpendLimitOverrideResponse' {monthlyLimit} -> monthlyLimit) (\s@SetVoiceMessageSpendLimitOverrideResponse' {} a -> s {monthlyLimit = a} :: SetVoiceMessageSpendLimitOverrideResponse)

-- | The response's http status code.
setVoiceMessageSpendLimitOverrideResponse_httpStatus :: Lens.Lens' SetVoiceMessageSpendLimitOverrideResponse Prelude.Int
setVoiceMessageSpendLimitOverrideResponse_httpStatus = Lens.lens (\SetVoiceMessageSpendLimitOverrideResponse' {httpStatus} -> httpStatus) (\s@SetVoiceMessageSpendLimitOverrideResponse' {} a -> s {httpStatus = a} :: SetVoiceMessageSpendLimitOverrideResponse)

instance
  Prelude.NFData
    SetVoiceMessageSpendLimitOverrideResponse
  where
  rnf SetVoiceMessageSpendLimitOverrideResponse' {..} =
    Prelude.rnf monthlyLimit
      `Prelude.seq` Prelude.rnf httpStatus
