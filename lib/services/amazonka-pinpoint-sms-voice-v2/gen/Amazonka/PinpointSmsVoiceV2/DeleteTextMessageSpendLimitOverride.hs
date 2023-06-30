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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteTextMessageSpendLimitOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an account-level monthly spending limit override for sending
-- text messages. Deleting a spend limit override will set the
-- @EnforcedLimit@ to equal the @MaxLimit@, which is controlled by Amazon
-- Web Services. For more information on spend limits (quotas) see
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/quotas.html Amazon Pinpoint quotas>
-- in the /Amazon Pinpoint Developer Guide/.
module Amazonka.PinpointSmsVoiceV2.DeleteTextMessageSpendLimitOverride
  ( -- * Creating a Request
    DeleteTextMessageSpendLimitOverride (..),
    newDeleteTextMessageSpendLimitOverride,

    -- * Destructuring the Response
    DeleteTextMessageSpendLimitOverrideResponse (..),
    newDeleteTextMessageSpendLimitOverrideResponse,

    -- * Response Lenses
    deleteTextMessageSpendLimitOverrideResponse_monthlyLimit,
    deleteTextMessageSpendLimitOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTextMessageSpendLimitOverride' smart constructor.
data DeleteTextMessageSpendLimitOverride = DeleteTextMessageSpendLimitOverride'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTextMessageSpendLimitOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTextMessageSpendLimitOverride ::
  DeleteTextMessageSpendLimitOverride
newDeleteTextMessageSpendLimitOverride =
  DeleteTextMessageSpendLimitOverride'

instance
  Core.AWSRequest
    DeleteTextMessageSpendLimitOverride
  where
  type
    AWSResponse DeleteTextMessageSpendLimitOverride =
      DeleteTextMessageSpendLimitOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTextMessageSpendLimitOverrideResponse'
            Prelude.<$> (x Data..?> "MonthlyLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTextMessageSpendLimitOverride
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DeleteTextMessageSpendLimitOverride
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DeleteTextMessageSpendLimitOverride
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteTextMessageSpendLimitOverride" ::
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
    DeleteTextMessageSpendLimitOverride
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    DeleteTextMessageSpendLimitOverride
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteTextMessageSpendLimitOverride
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTextMessageSpendLimitOverrideResponse' smart constructor.
data DeleteTextMessageSpendLimitOverrideResponse = DeleteTextMessageSpendLimitOverrideResponse'
  { -- | The current monthly limit, in US dollars.
    monthlyLimit :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTextMessageSpendLimitOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monthlyLimit', 'deleteTextMessageSpendLimitOverrideResponse_monthlyLimit' - The current monthly limit, in US dollars.
--
-- 'httpStatus', 'deleteTextMessageSpendLimitOverrideResponse_httpStatus' - The response's http status code.
newDeleteTextMessageSpendLimitOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTextMessageSpendLimitOverrideResponse
newDeleteTextMessageSpendLimitOverrideResponse
  pHttpStatus_ =
    DeleteTextMessageSpendLimitOverrideResponse'
      { monthlyLimit =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current monthly limit, in US dollars.
deleteTextMessageSpendLimitOverrideResponse_monthlyLimit :: Lens.Lens' DeleteTextMessageSpendLimitOverrideResponse (Prelude.Maybe Prelude.Natural)
deleteTextMessageSpendLimitOverrideResponse_monthlyLimit = Lens.lens (\DeleteTextMessageSpendLimitOverrideResponse' {monthlyLimit} -> monthlyLimit) (\s@DeleteTextMessageSpendLimitOverrideResponse' {} a -> s {monthlyLimit = a} :: DeleteTextMessageSpendLimitOverrideResponse)

-- | The response's http status code.
deleteTextMessageSpendLimitOverrideResponse_httpStatus :: Lens.Lens' DeleteTextMessageSpendLimitOverrideResponse Prelude.Int
deleteTextMessageSpendLimitOverrideResponse_httpStatus = Lens.lens (\DeleteTextMessageSpendLimitOverrideResponse' {httpStatus} -> httpStatus) (\s@DeleteTextMessageSpendLimitOverrideResponse' {} a -> s {httpStatus = a} :: DeleteTextMessageSpendLimitOverrideResponse)

instance
  Prelude.NFData
    DeleteTextMessageSpendLimitOverrideResponse
  where
  rnf DeleteTextMessageSpendLimitOverrideResponse' {..} =
    Prelude.rnf monthlyLimit
      `Prelude.seq` Prelude.rnf httpStatus
