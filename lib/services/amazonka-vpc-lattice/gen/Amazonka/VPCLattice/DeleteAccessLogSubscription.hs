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
-- Module      : Amazonka.VPCLattice.DeleteAccessLogSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified access log subscription.
module Amazonka.VPCLattice.DeleteAccessLogSubscription
  ( -- * Creating a Request
    DeleteAccessLogSubscription (..),
    newDeleteAccessLogSubscription,

    -- * Request Lenses
    deleteAccessLogSubscription_accessLogSubscriptionIdentifier,

    -- * Destructuring the Response
    DeleteAccessLogSubscriptionResponse (..),
    newDeleteAccessLogSubscriptionResponse,

    -- * Response Lenses
    deleteAccessLogSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteAccessLogSubscription' smart constructor.
data DeleteAccessLogSubscription = DeleteAccessLogSubscription'
  { -- | The ID or Amazon Resource Name (ARN) of the access log subscription.
    accessLogSubscriptionIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessLogSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogSubscriptionIdentifier', 'deleteAccessLogSubscription_accessLogSubscriptionIdentifier' - The ID or Amazon Resource Name (ARN) of the access log subscription.
newDeleteAccessLogSubscription ::
  -- | 'accessLogSubscriptionIdentifier'
  Prelude.Text ->
  DeleteAccessLogSubscription
newDeleteAccessLogSubscription
  pAccessLogSubscriptionIdentifier_ =
    DeleteAccessLogSubscription'
      { accessLogSubscriptionIdentifier =
          pAccessLogSubscriptionIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the access log subscription.
deleteAccessLogSubscription_accessLogSubscriptionIdentifier :: Lens.Lens' DeleteAccessLogSubscription Prelude.Text
deleteAccessLogSubscription_accessLogSubscriptionIdentifier = Lens.lens (\DeleteAccessLogSubscription' {accessLogSubscriptionIdentifier} -> accessLogSubscriptionIdentifier) (\s@DeleteAccessLogSubscription' {} a -> s {accessLogSubscriptionIdentifier = a} :: DeleteAccessLogSubscription)

instance Core.AWSRequest DeleteAccessLogSubscription where
  type
    AWSResponse DeleteAccessLogSubscription =
      DeleteAccessLogSubscriptionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessLogSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccessLogSubscription where
  hashWithSalt _salt DeleteAccessLogSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` accessLogSubscriptionIdentifier

instance Prelude.NFData DeleteAccessLogSubscription where
  rnf DeleteAccessLogSubscription' {..} =
    Prelude.rnf accessLogSubscriptionIdentifier

instance Data.ToHeaders DeleteAccessLogSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAccessLogSubscription where
  toPath DeleteAccessLogSubscription' {..} =
    Prelude.mconcat
      [ "/accesslogsubscriptions/",
        Data.toBS accessLogSubscriptionIdentifier
      ]

instance Data.ToQuery DeleteAccessLogSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessLogSubscriptionResponse' smart constructor.
data DeleteAccessLogSubscriptionResponse = DeleteAccessLogSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessLogSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccessLogSubscriptionResponse_httpStatus' - The response's http status code.
newDeleteAccessLogSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccessLogSubscriptionResponse
newDeleteAccessLogSubscriptionResponse pHttpStatus_ =
  DeleteAccessLogSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAccessLogSubscriptionResponse_httpStatus :: Lens.Lens' DeleteAccessLogSubscriptionResponse Prelude.Int
deleteAccessLogSubscriptionResponse_httpStatus = Lens.lens (\DeleteAccessLogSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteAccessLogSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteAccessLogSubscriptionResponse)

instance
  Prelude.NFData
    DeleteAccessLogSubscriptionResponse
  where
  rnf DeleteAccessLogSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
