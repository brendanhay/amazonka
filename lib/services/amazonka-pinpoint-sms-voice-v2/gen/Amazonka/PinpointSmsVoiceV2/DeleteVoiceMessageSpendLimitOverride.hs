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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteVoiceMessageSpendLimitOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an account level monthly spend limit override for sending voice
-- messages. Deleting a spend limit override sets the @EnforcedLimit@ equal
-- to the @MaxLimit@, which is controlled by Amazon Web Services. For more
-- information on spending limits (quotas) see
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/quotas.html Amazon Pinpoint quotas>
-- in the /Amazon Pinpoint Developer Guide/.
module Amazonka.PinpointSmsVoiceV2.DeleteVoiceMessageSpendLimitOverride
  ( -- * Creating a Request
    DeleteVoiceMessageSpendLimitOverride (..),
    newDeleteVoiceMessageSpendLimitOverride,

    -- * Destructuring the Response
    DeleteVoiceMessageSpendLimitOverrideResponse (..),
    newDeleteVoiceMessageSpendLimitOverrideResponse,

    -- * Response Lenses
    deleteVoiceMessageSpendLimitOverrideResponse_monthlyLimit,
    deleteVoiceMessageSpendLimitOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceMessageSpendLimitOverride' smart constructor.
data DeleteVoiceMessageSpendLimitOverride = DeleteVoiceMessageSpendLimitOverride'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceMessageSpendLimitOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVoiceMessageSpendLimitOverride ::
  DeleteVoiceMessageSpendLimitOverride
newDeleteVoiceMessageSpendLimitOverride =
  DeleteVoiceMessageSpendLimitOverride'

instance
  Core.AWSRequest
    DeleteVoiceMessageSpendLimitOverride
  where
  type
    AWSResponse DeleteVoiceMessageSpendLimitOverride =
      DeleteVoiceMessageSpendLimitOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVoiceMessageSpendLimitOverrideResponse'
            Prelude.<$> (x Data..?> "MonthlyLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVoiceMessageSpendLimitOverride
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DeleteVoiceMessageSpendLimitOverride
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DeleteVoiceMessageSpendLimitOverride
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteVoiceMessageSpendLimitOverride" ::
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
    DeleteVoiceMessageSpendLimitOverride
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    DeleteVoiceMessageSpendLimitOverride
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteVoiceMessageSpendLimitOverride
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVoiceMessageSpendLimitOverrideResponse' smart constructor.
data DeleteVoiceMessageSpendLimitOverrideResponse = DeleteVoiceMessageSpendLimitOverrideResponse'
  { -- | The current monthly limit, in US dollars.
    monthlyLimit :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceMessageSpendLimitOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monthlyLimit', 'deleteVoiceMessageSpendLimitOverrideResponse_monthlyLimit' - The current monthly limit, in US dollars.
--
-- 'httpStatus', 'deleteVoiceMessageSpendLimitOverrideResponse_httpStatus' - The response's http status code.
newDeleteVoiceMessageSpendLimitOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVoiceMessageSpendLimitOverrideResponse
newDeleteVoiceMessageSpendLimitOverrideResponse
  pHttpStatus_ =
    DeleteVoiceMessageSpendLimitOverrideResponse'
      { monthlyLimit =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current monthly limit, in US dollars.
deleteVoiceMessageSpendLimitOverrideResponse_monthlyLimit :: Lens.Lens' DeleteVoiceMessageSpendLimitOverrideResponse (Prelude.Maybe Prelude.Natural)
deleteVoiceMessageSpendLimitOverrideResponse_monthlyLimit = Lens.lens (\DeleteVoiceMessageSpendLimitOverrideResponse' {monthlyLimit} -> monthlyLimit) (\s@DeleteVoiceMessageSpendLimitOverrideResponse' {} a -> s {monthlyLimit = a} :: DeleteVoiceMessageSpendLimitOverrideResponse)

-- | The response's http status code.
deleteVoiceMessageSpendLimitOverrideResponse_httpStatus :: Lens.Lens' DeleteVoiceMessageSpendLimitOverrideResponse Prelude.Int
deleteVoiceMessageSpendLimitOverrideResponse_httpStatus = Lens.lens (\DeleteVoiceMessageSpendLimitOverrideResponse' {httpStatus} -> httpStatus) (\s@DeleteVoiceMessageSpendLimitOverrideResponse' {} a -> s {httpStatus = a} :: DeleteVoiceMessageSpendLimitOverrideResponse)

instance
  Prelude.NFData
    DeleteVoiceMessageSpendLimitOverrideResponse
  where
  rnf DeleteVoiceMessageSpendLimitOverrideResponse' {..} =
    Prelude.rnf monthlyLimit `Prelude.seq`
      Prelude.rnf httpStatus
