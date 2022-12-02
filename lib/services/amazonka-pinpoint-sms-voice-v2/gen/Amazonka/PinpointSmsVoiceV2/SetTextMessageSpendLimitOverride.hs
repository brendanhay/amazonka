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
-- Module      : Amazonka.PinpointSmsVoiceV2.SetTextMessageSpendLimitOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an account level monthly spend limit override for sending text
-- messages. The requested spend limit must be less than or equal to the
-- @MaxLimit@, which is set by Amazon Web Services.
module Amazonka.PinpointSmsVoiceV2.SetTextMessageSpendLimitOverride
  ( -- * Creating a Request
    SetTextMessageSpendLimitOverride (..),
    newSetTextMessageSpendLimitOverride,

    -- * Request Lenses
    setTextMessageSpendLimitOverride_monthlyLimit,

    -- * Destructuring the Response
    SetTextMessageSpendLimitOverrideResponse (..),
    newSetTextMessageSpendLimitOverrideResponse,

    -- * Response Lenses
    setTextMessageSpendLimitOverrideResponse_monthlyLimit,
    setTextMessageSpendLimitOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetTextMessageSpendLimitOverride' smart constructor.
data SetTextMessageSpendLimitOverride = SetTextMessageSpendLimitOverride'
  { -- | The new monthly limit to enforce on text messages.
    monthlyLimit :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTextMessageSpendLimitOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monthlyLimit', 'setTextMessageSpendLimitOverride_monthlyLimit' - The new monthly limit to enforce on text messages.
newSetTextMessageSpendLimitOverride ::
  -- | 'monthlyLimit'
  Prelude.Natural ->
  SetTextMessageSpendLimitOverride
newSetTextMessageSpendLimitOverride pMonthlyLimit_ =
  SetTextMessageSpendLimitOverride'
    { monthlyLimit =
        pMonthlyLimit_
    }

-- | The new monthly limit to enforce on text messages.
setTextMessageSpendLimitOverride_monthlyLimit :: Lens.Lens' SetTextMessageSpendLimitOverride Prelude.Natural
setTextMessageSpendLimitOverride_monthlyLimit = Lens.lens (\SetTextMessageSpendLimitOverride' {monthlyLimit} -> monthlyLimit) (\s@SetTextMessageSpendLimitOverride' {} a -> s {monthlyLimit = a} :: SetTextMessageSpendLimitOverride)

instance
  Core.AWSRequest
    SetTextMessageSpendLimitOverride
  where
  type
    AWSResponse SetTextMessageSpendLimitOverride =
      SetTextMessageSpendLimitOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetTextMessageSpendLimitOverrideResponse'
            Prelude.<$> (x Data..?> "MonthlyLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetTextMessageSpendLimitOverride
  where
  hashWithSalt
    _salt
    SetTextMessageSpendLimitOverride' {..} =
      _salt `Prelude.hashWithSalt` monthlyLimit

instance
  Prelude.NFData
    SetTextMessageSpendLimitOverride
  where
  rnf SetTextMessageSpendLimitOverride' {..} =
    Prelude.rnf monthlyLimit

instance
  Data.ToHeaders
    SetTextMessageSpendLimitOverride
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.SetTextMessageSpendLimitOverride" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetTextMessageSpendLimitOverride where
  toJSON SetTextMessageSpendLimitOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MonthlyLimit" Data..= monthlyLimit)]
      )

instance Data.ToPath SetTextMessageSpendLimitOverride where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    SetTextMessageSpendLimitOverride
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetTextMessageSpendLimitOverrideResponse' smart constructor.
data SetTextMessageSpendLimitOverrideResponse = SetTextMessageSpendLimitOverrideResponse'
  { -- | The current monthly limit to enforce on sending text messages.
    monthlyLimit :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTextMessageSpendLimitOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monthlyLimit', 'setTextMessageSpendLimitOverrideResponse_monthlyLimit' - The current monthly limit to enforce on sending text messages.
--
-- 'httpStatus', 'setTextMessageSpendLimitOverrideResponse_httpStatus' - The response's http status code.
newSetTextMessageSpendLimitOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetTextMessageSpendLimitOverrideResponse
newSetTextMessageSpendLimitOverrideResponse
  pHttpStatus_ =
    SetTextMessageSpendLimitOverrideResponse'
      { monthlyLimit =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current monthly limit to enforce on sending text messages.
setTextMessageSpendLimitOverrideResponse_monthlyLimit :: Lens.Lens' SetTextMessageSpendLimitOverrideResponse (Prelude.Maybe Prelude.Natural)
setTextMessageSpendLimitOverrideResponse_monthlyLimit = Lens.lens (\SetTextMessageSpendLimitOverrideResponse' {monthlyLimit} -> monthlyLimit) (\s@SetTextMessageSpendLimitOverrideResponse' {} a -> s {monthlyLimit = a} :: SetTextMessageSpendLimitOverrideResponse)

-- | The response's http status code.
setTextMessageSpendLimitOverrideResponse_httpStatus :: Lens.Lens' SetTextMessageSpendLimitOverrideResponse Prelude.Int
setTextMessageSpendLimitOverrideResponse_httpStatus = Lens.lens (\SetTextMessageSpendLimitOverrideResponse' {httpStatus} -> httpStatus) (\s@SetTextMessageSpendLimitOverrideResponse' {} a -> s {httpStatus = a} :: SetTextMessageSpendLimitOverrideResponse)

instance
  Prelude.NFData
    SetTextMessageSpendLimitOverrideResponse
  where
  rnf SetTextMessageSpendLimitOverrideResponse' {..} =
    Prelude.rnf monthlyLimit
      `Prelude.seq` Prelude.rnf httpStatus
