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
-- Module      : Amazonka.QuickSight.DeleteAccountSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @DeleteAccountSubscription@ operation to delete an Amazon
-- QuickSight account. This operation will result in an error message if
-- you have configured your account termination protection settings to
-- @True@. To change this setting and delete your account, call the
-- @UpdateAccountSettings@ API and set the value of the
-- @TerminationProtectionEnabled@ parameter to @False@, then make another
-- call to the @DeleteAccountSubscription@ API.
module Amazonka.QuickSight.DeleteAccountSubscription
  ( -- * Creating a Request
    DeleteAccountSubscription (..),
    newDeleteAccountSubscription,

    -- * Request Lenses
    deleteAccountSubscription_awsAccountId,

    -- * Destructuring the Response
    DeleteAccountSubscriptionResponse (..),
    newDeleteAccountSubscriptionResponse,

    -- * Response Lenses
    deleteAccountSubscriptionResponse_requestId,
    deleteAccountSubscriptionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccountSubscription' smart constructor.
data DeleteAccountSubscription = DeleteAccountSubscription'
  { -- | The Amazon Web Services account ID of the account that you want to
    -- delete.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteAccountSubscription_awsAccountId' - The Amazon Web Services account ID of the account that you want to
-- delete.
newDeleteAccountSubscription ::
  -- | 'awsAccountId'
  Prelude.Text ->
  DeleteAccountSubscription
newDeleteAccountSubscription pAwsAccountId_ =
  DeleteAccountSubscription'
    { awsAccountId =
        pAwsAccountId_
    }

-- | The Amazon Web Services account ID of the account that you want to
-- delete.
deleteAccountSubscription_awsAccountId :: Lens.Lens' DeleteAccountSubscription Prelude.Text
deleteAccountSubscription_awsAccountId = Lens.lens (\DeleteAccountSubscription' {awsAccountId} -> awsAccountId) (\s@DeleteAccountSubscription' {} a -> s {awsAccountId = a} :: DeleteAccountSubscription)

instance Core.AWSRequest DeleteAccountSubscription where
  type
    AWSResponse DeleteAccountSubscription =
      DeleteAccountSubscriptionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAccountSubscriptionResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccountSubscription where
  hashWithSalt _salt DeleteAccountSubscription' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData DeleteAccountSubscription where
  rnf DeleteAccountSubscription' {..} =
    Prelude.rnf awsAccountId

instance Data.ToHeaders DeleteAccountSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAccountSubscription where
  toPath DeleteAccountSubscription' {..} =
    Prelude.mconcat
      ["/account/", Data.toBS awsAccountId]

instance Data.ToQuery DeleteAccountSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccountSubscriptionResponse' smart constructor.
data DeleteAccountSubscriptionResponse = DeleteAccountSubscriptionResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteAccountSubscriptionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteAccountSubscriptionResponse_status' - The HTTP status of the request.
newDeleteAccountSubscriptionResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteAccountSubscriptionResponse
newDeleteAccountSubscriptionResponse pStatus_ =
  DeleteAccountSubscriptionResponse'
    { requestId =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteAccountSubscriptionResponse_requestId :: Lens.Lens' DeleteAccountSubscriptionResponse (Prelude.Maybe Prelude.Text)
deleteAccountSubscriptionResponse_requestId = Lens.lens (\DeleteAccountSubscriptionResponse' {requestId} -> requestId) (\s@DeleteAccountSubscriptionResponse' {} a -> s {requestId = a} :: DeleteAccountSubscriptionResponse)

-- | The HTTP status of the request.
deleteAccountSubscriptionResponse_status :: Lens.Lens' DeleteAccountSubscriptionResponse Prelude.Int
deleteAccountSubscriptionResponse_status = Lens.lens (\DeleteAccountSubscriptionResponse' {status} -> status) (\s@DeleteAccountSubscriptionResponse' {} a -> s {status = a} :: DeleteAccountSubscriptionResponse)

instance
  Prelude.NFData
    DeleteAccountSubscriptionResponse
  where
  rnf DeleteAccountSubscriptionResponse' {..} =
    Prelude.rnf requestId `Prelude.seq`
      Prelude.rnf status
