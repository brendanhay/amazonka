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
-- Module      : Amazonka.Connect.DescribePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details and status of a phone number that’s claimed to your Amazon
-- Connect instance or traffic distribution group.
--
-- If the number is claimed to a traffic distribution group, and you are
-- calling in the Amazon Web Services Region where the traffic distribution
-- group was created, you can use either a phone number ARN or UUID value
-- for the @PhoneNumberId@ URI request parameter. However, if the number is
-- claimed to a traffic distribution group and you are calling this API in
-- the alternate Amazon Web Services Region associated with the traffic
-- distribution group, you must provide a full phone number ARN. If a UUID
-- is provided in this scenario, you will receive a
-- @ResourceNotFoundException@.
module Amazonka.Connect.DescribePhoneNumber
  ( -- * Creating a Request
    DescribePhoneNumber (..),
    newDescribePhoneNumber,

    -- * Request Lenses
    describePhoneNumber_phoneNumberId,

    -- * Destructuring the Response
    DescribePhoneNumberResponse (..),
    newDescribePhoneNumberResponse,

    -- * Response Lenses
    describePhoneNumberResponse_claimedPhoneNumberSummary,
    describePhoneNumberResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePhoneNumber' smart constructor.
data DescribePhoneNumber = DescribePhoneNumber'
  { -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'describePhoneNumber_phoneNumberId' - A unique identifier for the phone number.
newDescribePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  DescribePhoneNumber
newDescribePhoneNumber pPhoneNumberId_ =
  DescribePhoneNumber'
    { phoneNumberId =
        pPhoneNumberId_
    }

-- | A unique identifier for the phone number.
describePhoneNumber_phoneNumberId :: Lens.Lens' DescribePhoneNumber Prelude.Text
describePhoneNumber_phoneNumberId = Lens.lens (\DescribePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@DescribePhoneNumber' {} a -> s {phoneNumberId = a} :: DescribePhoneNumber)

instance Core.AWSRequest DescribePhoneNumber where
  type
    AWSResponse DescribePhoneNumber =
      DescribePhoneNumberResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePhoneNumberResponse'
            Prelude.<$> (x Data..?> "ClaimedPhoneNumberSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePhoneNumber where
  hashWithSalt _salt DescribePhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData DescribePhoneNumber where
  rnf DescribePhoneNumber' {..} =
    Prelude.rnf phoneNumberId

instance Data.ToHeaders DescribePhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePhoneNumber where
  toPath DescribePhoneNumber' {..} =
    Prelude.mconcat
      ["/phone-number/", Data.toBS phoneNumberId]

instance Data.ToQuery DescribePhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePhoneNumberResponse' smart constructor.
data DescribePhoneNumberResponse = DescribePhoneNumberResponse'
  { -- | Information about a phone number that\'s been claimed to your Amazon
    -- Connect instance or traffic distribution group.
    claimedPhoneNumberSummary :: Prelude.Maybe ClaimedPhoneNumberSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'claimedPhoneNumberSummary', 'describePhoneNumberResponse_claimedPhoneNumberSummary' - Information about a phone number that\'s been claimed to your Amazon
-- Connect instance or traffic distribution group.
--
-- 'httpStatus', 'describePhoneNumberResponse_httpStatus' - The response's http status code.
newDescribePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePhoneNumberResponse
newDescribePhoneNumberResponse pHttpStatus_ =
  DescribePhoneNumberResponse'
    { claimedPhoneNumberSummary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a phone number that\'s been claimed to your Amazon
-- Connect instance or traffic distribution group.
describePhoneNumberResponse_claimedPhoneNumberSummary :: Lens.Lens' DescribePhoneNumberResponse (Prelude.Maybe ClaimedPhoneNumberSummary)
describePhoneNumberResponse_claimedPhoneNumberSummary = Lens.lens (\DescribePhoneNumberResponse' {claimedPhoneNumberSummary} -> claimedPhoneNumberSummary) (\s@DescribePhoneNumberResponse' {} a -> s {claimedPhoneNumberSummary = a} :: DescribePhoneNumberResponse)

-- | The response's http status code.
describePhoneNumberResponse_httpStatus :: Lens.Lens' DescribePhoneNumberResponse Prelude.Int
describePhoneNumberResponse_httpStatus = Lens.lens (\DescribePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@DescribePhoneNumberResponse' {} a -> s {httpStatus = a} :: DescribePhoneNumberResponse)

instance Prelude.NFData DescribePhoneNumberResponse where
  rnf DescribePhoneNumberResponse' {..} =
    Prelude.rnf claimedPhoneNumberSummary
      `Prelude.seq` Prelude.rnf httpStatus
