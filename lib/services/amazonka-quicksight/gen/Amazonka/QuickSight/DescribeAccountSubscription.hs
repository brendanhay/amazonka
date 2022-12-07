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
-- Module      : Amazonka.QuickSight.DescribeAccountSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the DescribeAccountSubscription operation to receive a description
-- of an Amazon QuickSight account\'s subscription. A successful API call
-- returns an @AccountInfo@ object that includes an account\'s name,
-- subscription status, authentication type, edition, and notification
-- email address.
module Amazonka.QuickSight.DescribeAccountSubscription
  ( -- * Creating a Request
    DescribeAccountSubscription (..),
    newDescribeAccountSubscription,

    -- * Request Lenses
    describeAccountSubscription_awsAccountId,

    -- * Destructuring the Response
    DescribeAccountSubscriptionResponse (..),
    newDescribeAccountSubscriptionResponse,

    -- * Response Lenses
    describeAccountSubscriptionResponse_accountInfo,
    describeAccountSubscriptionResponse_requestId,
    describeAccountSubscriptionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountSubscription' smart constructor.
data DescribeAccountSubscription = DescribeAccountSubscription'
  { -- | The Amazon Web Services account ID associated with your Amazon
    -- QuickSight account.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAccountSubscription_awsAccountId' - The Amazon Web Services account ID associated with your Amazon
-- QuickSight account.
newDescribeAccountSubscription ::
  -- | 'awsAccountId'
  Prelude.Text ->
  DescribeAccountSubscription
newDescribeAccountSubscription pAwsAccountId_ =
  DescribeAccountSubscription'
    { awsAccountId =
        pAwsAccountId_
    }

-- | The Amazon Web Services account ID associated with your Amazon
-- QuickSight account.
describeAccountSubscription_awsAccountId :: Lens.Lens' DescribeAccountSubscription Prelude.Text
describeAccountSubscription_awsAccountId = Lens.lens (\DescribeAccountSubscription' {awsAccountId} -> awsAccountId) (\s@DescribeAccountSubscription' {} a -> s {awsAccountId = a} :: DescribeAccountSubscription)

instance Core.AWSRequest DescribeAccountSubscription where
  type
    AWSResponse DescribeAccountSubscription =
      DescribeAccountSubscriptionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountSubscriptionResponse'
            Prelude.<$> (x Data..?> "AccountInfo")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountSubscription where
  hashWithSalt _salt DescribeAccountSubscription' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData DescribeAccountSubscription where
  rnf DescribeAccountSubscription' {..} =
    Prelude.rnf awsAccountId

instance Data.ToHeaders DescribeAccountSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAccountSubscription where
  toPath DescribeAccountSubscription' {..} =
    Prelude.mconcat
      ["/account/", Data.toBS awsAccountId]

instance Data.ToQuery DescribeAccountSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountSubscriptionResponse' smart constructor.
data DescribeAccountSubscriptionResponse = DescribeAccountSubscriptionResponse'
  { -- | A structure that contains the following elements:
    --
    -- -   Your Amazon QuickSight account name.
    --
    -- -   The edition of Amazon QuickSight that your account is using.
    --
    -- -   The notification email address that is associated with the Amazon
    --     QuickSight account.
    --
    -- -   The authentication type of the Amazon QuickSight account.
    --
    -- -   The status of the Amazon QuickSight account\'s subscription.
    accountInfo :: Prelude.Maybe AccountInfo,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountInfo', 'describeAccountSubscriptionResponse_accountInfo' - A structure that contains the following elements:
--
-- -   Your Amazon QuickSight account name.
--
-- -   The edition of Amazon QuickSight that your account is using.
--
-- -   The notification email address that is associated with the Amazon
--     QuickSight account.
--
-- -   The authentication type of the Amazon QuickSight account.
--
-- -   The status of the Amazon QuickSight account\'s subscription.
--
-- 'requestId', 'describeAccountSubscriptionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeAccountSubscriptionResponse_status' - The HTTP status of the request.
newDescribeAccountSubscriptionResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeAccountSubscriptionResponse
newDescribeAccountSubscriptionResponse pStatus_ =
  DescribeAccountSubscriptionResponse'
    { accountInfo =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A structure that contains the following elements:
--
-- -   Your Amazon QuickSight account name.
--
-- -   The edition of Amazon QuickSight that your account is using.
--
-- -   The notification email address that is associated with the Amazon
--     QuickSight account.
--
-- -   The authentication type of the Amazon QuickSight account.
--
-- -   The status of the Amazon QuickSight account\'s subscription.
describeAccountSubscriptionResponse_accountInfo :: Lens.Lens' DescribeAccountSubscriptionResponse (Prelude.Maybe AccountInfo)
describeAccountSubscriptionResponse_accountInfo = Lens.lens (\DescribeAccountSubscriptionResponse' {accountInfo} -> accountInfo) (\s@DescribeAccountSubscriptionResponse' {} a -> s {accountInfo = a} :: DescribeAccountSubscriptionResponse)

-- | The Amazon Web Services request ID for this operation.
describeAccountSubscriptionResponse_requestId :: Lens.Lens' DescribeAccountSubscriptionResponse (Prelude.Maybe Prelude.Text)
describeAccountSubscriptionResponse_requestId = Lens.lens (\DescribeAccountSubscriptionResponse' {requestId} -> requestId) (\s@DescribeAccountSubscriptionResponse' {} a -> s {requestId = a} :: DescribeAccountSubscriptionResponse)

-- | The HTTP status of the request.
describeAccountSubscriptionResponse_status :: Lens.Lens' DescribeAccountSubscriptionResponse Prelude.Int
describeAccountSubscriptionResponse_status = Lens.lens (\DescribeAccountSubscriptionResponse' {status} -> status) (\s@DescribeAccountSubscriptionResponse' {} a -> s {status = a} :: DescribeAccountSubscriptionResponse)

instance
  Prelude.NFData
    DescribeAccountSubscriptionResponse
  where
  rnf DescribeAccountSubscriptionResponse' {..} =
    Prelude.rnf accountInfo
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
