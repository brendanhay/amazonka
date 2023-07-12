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
-- Module      : Amazonka.ChimeSdkVoice.BatchUpdatePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.BatchUpdatePhoneNumber
  ( -- * Creating a Request
    BatchUpdatePhoneNumber (..),
    newBatchUpdatePhoneNumber,

    -- * Request Lenses
    batchUpdatePhoneNumber_updatePhoneNumberRequestItems,

    -- * Destructuring the Response
    BatchUpdatePhoneNumberResponse (..),
    newBatchUpdatePhoneNumberResponse,

    -- * Response Lenses
    batchUpdatePhoneNumberResponse_phoneNumberErrors,
    batchUpdatePhoneNumberResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdatePhoneNumber' smart constructor.
data BatchUpdatePhoneNumber = BatchUpdatePhoneNumber'
  { updatePhoneNumberRequestItems :: [UpdatePhoneNumberRequestItem]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdatePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updatePhoneNumberRequestItems', 'batchUpdatePhoneNumber_updatePhoneNumberRequestItems' - Undocumented member.
newBatchUpdatePhoneNumber ::
  BatchUpdatePhoneNumber
newBatchUpdatePhoneNumber =
  BatchUpdatePhoneNumber'
    { updatePhoneNumberRequestItems =
        Prelude.mempty
    }

-- | Undocumented member.
batchUpdatePhoneNumber_updatePhoneNumberRequestItems :: Lens.Lens' BatchUpdatePhoneNumber [UpdatePhoneNumberRequestItem]
batchUpdatePhoneNumber_updatePhoneNumberRequestItems = Lens.lens (\BatchUpdatePhoneNumber' {updatePhoneNumberRequestItems} -> updatePhoneNumberRequestItems) (\s@BatchUpdatePhoneNumber' {} a -> s {updatePhoneNumberRequestItems = a} :: BatchUpdatePhoneNumber) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpdatePhoneNumber where
  type
    AWSResponse BatchUpdatePhoneNumber =
      BatchUpdatePhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdatePhoneNumberResponse'
            Prelude.<$> ( x
                            Data..?> "PhoneNumberErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdatePhoneNumber where
  hashWithSalt _salt BatchUpdatePhoneNumber' {..} =
    _salt
      `Prelude.hashWithSalt` updatePhoneNumberRequestItems

instance Prelude.NFData BatchUpdatePhoneNumber where
  rnf BatchUpdatePhoneNumber' {..} =
    Prelude.rnf updatePhoneNumberRequestItems

instance Data.ToHeaders BatchUpdatePhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchUpdatePhoneNumber where
  toJSON BatchUpdatePhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UpdatePhoneNumberRequestItems"
                  Data..= updatePhoneNumberRequestItems
              )
          ]
      )

instance Data.ToPath BatchUpdatePhoneNumber where
  toPath = Prelude.const "/phone-numbers"

instance Data.ToQuery BatchUpdatePhoneNumber where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=batch-update"])

-- | /See:/ 'newBatchUpdatePhoneNumberResponse' smart constructor.
data BatchUpdatePhoneNumberResponse = BatchUpdatePhoneNumberResponse'
  { phoneNumberErrors :: Prelude.Maybe [PhoneNumberError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdatePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberErrors', 'batchUpdatePhoneNumberResponse_phoneNumberErrors' - Undocumented member.
--
-- 'httpStatus', 'batchUpdatePhoneNumberResponse_httpStatus' - The response's http status code.
newBatchUpdatePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdatePhoneNumberResponse
newBatchUpdatePhoneNumberResponse pHttpStatus_ =
  BatchUpdatePhoneNumberResponse'
    { phoneNumberErrors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
batchUpdatePhoneNumberResponse_phoneNumberErrors :: Lens.Lens' BatchUpdatePhoneNumberResponse (Prelude.Maybe [PhoneNumberError])
batchUpdatePhoneNumberResponse_phoneNumberErrors = Lens.lens (\BatchUpdatePhoneNumberResponse' {phoneNumberErrors} -> phoneNumberErrors) (\s@BatchUpdatePhoneNumberResponse' {} a -> s {phoneNumberErrors = a} :: BatchUpdatePhoneNumberResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdatePhoneNumberResponse_httpStatus :: Lens.Lens' BatchUpdatePhoneNumberResponse Prelude.Int
batchUpdatePhoneNumberResponse_httpStatus = Lens.lens (\BatchUpdatePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@BatchUpdatePhoneNumberResponse' {} a -> s {httpStatus = a} :: BatchUpdatePhoneNumberResponse)

instance
  Prelude.NFData
    BatchUpdatePhoneNumberResponse
  where
  rnf BatchUpdatePhoneNumberResponse' {..} =
    Prelude.rnf phoneNumberErrors
      `Prelude.seq` Prelude.rnf httpStatus
