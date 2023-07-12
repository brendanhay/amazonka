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
-- Module      : Amazonka.Batch.DeleteSchedulingPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scheduling policy.
--
-- You can\'t delete a scheduling policy that\'s used in any job queues.
module Amazonka.Batch.DeleteSchedulingPolicy
  ( -- * Creating a Request
    DeleteSchedulingPolicy (..),
    newDeleteSchedulingPolicy,

    -- * Request Lenses
    deleteSchedulingPolicy_arn,

    -- * Destructuring the Response
    DeleteSchedulingPolicyResponse (..),
    newDeleteSchedulingPolicyResponse,

    -- * Response Lenses
    deleteSchedulingPolicyResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @DeleteSchedulingPolicy@.
--
-- /See:/ 'newDeleteSchedulingPolicy' smart constructor.
data DeleteSchedulingPolicy = DeleteSchedulingPolicy'
  { -- | The Amazon Resource Name (ARN) of the scheduling policy to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchedulingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteSchedulingPolicy_arn' - The Amazon Resource Name (ARN) of the scheduling policy to delete.
newDeleteSchedulingPolicy ::
  -- | 'arn'
  Prelude.Text ->
  DeleteSchedulingPolicy
newDeleteSchedulingPolicy pArn_ =
  DeleteSchedulingPolicy' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the scheduling policy to delete.
deleteSchedulingPolicy_arn :: Lens.Lens' DeleteSchedulingPolicy Prelude.Text
deleteSchedulingPolicy_arn = Lens.lens (\DeleteSchedulingPolicy' {arn} -> arn) (\s@DeleteSchedulingPolicy' {} a -> s {arn = a} :: DeleteSchedulingPolicy)

instance Core.AWSRequest DeleteSchedulingPolicy where
  type
    AWSResponse DeleteSchedulingPolicy =
      DeleteSchedulingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSchedulingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSchedulingPolicy where
  hashWithSalt _salt DeleteSchedulingPolicy' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteSchedulingPolicy where
  rnf DeleteSchedulingPolicy' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteSchedulingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSchedulingPolicy where
  toJSON DeleteSchedulingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteSchedulingPolicy where
  toPath = Prelude.const "/v1/deleteschedulingpolicy"

instance Data.ToQuery DeleteSchedulingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSchedulingPolicyResponse' smart constructor.
data DeleteSchedulingPolicyResponse = DeleteSchedulingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchedulingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSchedulingPolicyResponse_httpStatus' - The response's http status code.
newDeleteSchedulingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSchedulingPolicyResponse
newDeleteSchedulingPolicyResponse pHttpStatus_ =
  DeleteSchedulingPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSchedulingPolicyResponse_httpStatus :: Lens.Lens' DeleteSchedulingPolicyResponse Prelude.Int
deleteSchedulingPolicyResponse_httpStatus = Lens.lens (\DeleteSchedulingPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteSchedulingPolicyResponse' {} a -> s {httpStatus = a} :: DeleteSchedulingPolicyResponse)

instance
  Prelude.NFData
    DeleteSchedulingPolicyResponse
  where
  rnf DeleteSchedulingPolicyResponse' {..} =
    Prelude.rnf httpStatus
