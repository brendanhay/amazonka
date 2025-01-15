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
-- Module      : Amazonka.BillingConductor.DeleteBillingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a billing group.
module Amazonka.BillingConductor.DeleteBillingGroup
  ( -- * Creating a Request
    DeleteBillingGroup (..),
    newDeleteBillingGroup,

    -- * Request Lenses
    deleteBillingGroup_arn,

    -- * Destructuring the Response
    DeleteBillingGroupResponse (..),
    newDeleteBillingGroupResponse,

    -- * Response Lenses
    deleteBillingGroupResponse_arn,
    deleteBillingGroupResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBillingGroup' smart constructor.
data DeleteBillingGroup = DeleteBillingGroup'
  { -- | The Amazon Resource Name (ARN) of the billing group that you\'re
    -- deleting.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBillingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteBillingGroup_arn' - The Amazon Resource Name (ARN) of the billing group that you\'re
-- deleting.
newDeleteBillingGroup ::
  -- | 'arn'
  Prelude.Text ->
  DeleteBillingGroup
newDeleteBillingGroup pArn_ =
  DeleteBillingGroup' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the billing group that you\'re
-- deleting.
deleteBillingGroup_arn :: Lens.Lens' DeleteBillingGroup Prelude.Text
deleteBillingGroup_arn = Lens.lens (\DeleteBillingGroup' {arn} -> arn) (\s@DeleteBillingGroup' {} a -> s {arn = a} :: DeleteBillingGroup)

instance Core.AWSRequest DeleteBillingGroup where
  type
    AWSResponse DeleteBillingGroup =
      DeleteBillingGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBillingGroupResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBillingGroup where
  hashWithSalt _salt DeleteBillingGroup' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteBillingGroup where
  rnf DeleteBillingGroup' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteBillingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBillingGroup where
  toJSON DeleteBillingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath DeleteBillingGroup where
  toPath = Prelude.const "/delete-billing-group"

instance Data.ToQuery DeleteBillingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBillingGroupResponse' smart constructor.
data DeleteBillingGroupResponse = DeleteBillingGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted billing group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBillingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteBillingGroupResponse_arn' - The Amazon Resource Name (ARN) of the deleted billing group.
--
-- 'httpStatus', 'deleteBillingGroupResponse_httpStatus' - The response's http status code.
newDeleteBillingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBillingGroupResponse
newDeleteBillingGroupResponse pHttpStatus_ =
  DeleteBillingGroupResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted billing group.
deleteBillingGroupResponse_arn :: Lens.Lens' DeleteBillingGroupResponse (Prelude.Maybe Prelude.Text)
deleteBillingGroupResponse_arn = Lens.lens (\DeleteBillingGroupResponse' {arn} -> arn) (\s@DeleteBillingGroupResponse' {} a -> s {arn = a} :: DeleteBillingGroupResponse)

-- | The response's http status code.
deleteBillingGroupResponse_httpStatus :: Lens.Lens' DeleteBillingGroupResponse Prelude.Int
deleteBillingGroupResponse_httpStatus = Lens.lens (\DeleteBillingGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteBillingGroupResponse' {} a -> s {httpStatus = a} :: DeleteBillingGroupResponse)

instance Prelude.NFData DeleteBillingGroupResponse where
  rnf DeleteBillingGroupResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf httpStatus
