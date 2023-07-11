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
-- Module      : Amazonka.ChimeSdkVoice.BatchDeletePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.BatchDeletePhoneNumber
  ( -- * Creating a Request
    BatchDeletePhoneNumber (..),
    newBatchDeletePhoneNumber,

    -- * Request Lenses
    batchDeletePhoneNumber_phoneNumberIds,

    -- * Destructuring the Response
    BatchDeletePhoneNumberResponse (..),
    newBatchDeletePhoneNumberResponse,

    -- * Response Lenses
    batchDeletePhoneNumberResponse_phoneNumberErrors,
    batchDeletePhoneNumberResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeletePhoneNumber' smart constructor.
data BatchDeletePhoneNumber = BatchDeletePhoneNumber'
  { phoneNumberIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeletePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberIds', 'batchDeletePhoneNumber_phoneNumberIds' - Undocumented member.
newBatchDeletePhoneNumber ::
  -- | 'phoneNumberIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeletePhoneNumber
newBatchDeletePhoneNumber pPhoneNumberIds_ =
  BatchDeletePhoneNumber'
    { phoneNumberIds =
        Lens.coerced Lens.# pPhoneNumberIds_
    }

-- | Undocumented member.
batchDeletePhoneNumber_phoneNumberIds :: Lens.Lens' BatchDeletePhoneNumber (Prelude.NonEmpty Prelude.Text)
batchDeletePhoneNumber_phoneNumberIds = Lens.lens (\BatchDeletePhoneNumber' {phoneNumberIds} -> phoneNumberIds) (\s@BatchDeletePhoneNumber' {} a -> s {phoneNumberIds = a} :: BatchDeletePhoneNumber) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeletePhoneNumber where
  type
    AWSResponse BatchDeletePhoneNumber =
      BatchDeletePhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeletePhoneNumberResponse'
            Prelude.<$> ( x
                            Data..?> "PhoneNumberErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeletePhoneNumber where
  hashWithSalt _salt BatchDeletePhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberIds

instance Prelude.NFData BatchDeletePhoneNumber where
  rnf BatchDeletePhoneNumber' {..} =
    Prelude.rnf phoneNumberIds

instance Data.ToHeaders BatchDeletePhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchDeletePhoneNumber where
  toJSON BatchDeletePhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PhoneNumberIds" Data..= phoneNumberIds)
          ]
      )

instance Data.ToPath BatchDeletePhoneNumber where
  toPath = Prelude.const "/phone-numbers"

instance Data.ToQuery BatchDeletePhoneNumber where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=batch-delete"])

-- | /See:/ 'newBatchDeletePhoneNumberResponse' smart constructor.
data BatchDeletePhoneNumberResponse = BatchDeletePhoneNumberResponse'
  { phoneNumberErrors :: Prelude.Maybe [PhoneNumberError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeletePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberErrors', 'batchDeletePhoneNumberResponse_phoneNumberErrors' - Undocumented member.
--
-- 'httpStatus', 'batchDeletePhoneNumberResponse_httpStatus' - The response's http status code.
newBatchDeletePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeletePhoneNumberResponse
newBatchDeletePhoneNumberResponse pHttpStatus_ =
  BatchDeletePhoneNumberResponse'
    { phoneNumberErrors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
batchDeletePhoneNumberResponse_phoneNumberErrors :: Lens.Lens' BatchDeletePhoneNumberResponse (Prelude.Maybe [PhoneNumberError])
batchDeletePhoneNumberResponse_phoneNumberErrors = Lens.lens (\BatchDeletePhoneNumberResponse' {phoneNumberErrors} -> phoneNumberErrors) (\s@BatchDeletePhoneNumberResponse' {} a -> s {phoneNumberErrors = a} :: BatchDeletePhoneNumberResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeletePhoneNumberResponse_httpStatus :: Lens.Lens' BatchDeletePhoneNumberResponse Prelude.Int
batchDeletePhoneNumberResponse_httpStatus = Lens.lens (\BatchDeletePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@BatchDeletePhoneNumberResponse' {} a -> s {httpStatus = a} :: BatchDeletePhoneNumberResponse)

instance
  Prelude.NFData
    BatchDeletePhoneNumberResponse
  where
  rnf BatchDeletePhoneNumberResponse' {..} =
    Prelude.rnf phoneNumberErrors
      `Prelude.seq` Prelude.rnf httpStatus
