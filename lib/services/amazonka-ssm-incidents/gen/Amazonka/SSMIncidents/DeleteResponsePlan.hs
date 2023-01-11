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
-- Module      : Amazonka.SSMIncidents.DeleteResponsePlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified response plan. Deleting a response plan stops all
-- linked CloudWatch alarms and EventBridge events from creating an
-- incident with this response plan.
module Amazonka.SSMIncidents.DeleteResponsePlan
  ( -- * Creating a Request
    DeleteResponsePlan (..),
    newDeleteResponsePlan,

    -- * Request Lenses
    deleteResponsePlan_arn,

    -- * Destructuring the Response
    DeleteResponsePlanResponse (..),
    newDeleteResponsePlanResponse,

    -- * Response Lenses
    deleteResponsePlanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newDeleteResponsePlan' smart constructor.
data DeleteResponsePlan = DeleteResponsePlan'
  { -- | The Amazon Resource Name (ARN) of the response plan.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResponsePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteResponsePlan_arn' - The Amazon Resource Name (ARN) of the response plan.
newDeleteResponsePlan ::
  -- | 'arn'
  Prelude.Text ->
  DeleteResponsePlan
newDeleteResponsePlan pArn_ =
  DeleteResponsePlan' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the response plan.
deleteResponsePlan_arn :: Lens.Lens' DeleteResponsePlan Prelude.Text
deleteResponsePlan_arn = Lens.lens (\DeleteResponsePlan' {arn} -> arn) (\s@DeleteResponsePlan' {} a -> s {arn = a} :: DeleteResponsePlan)

instance Core.AWSRequest DeleteResponsePlan where
  type
    AWSResponse DeleteResponsePlan =
      DeleteResponsePlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResponsePlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResponsePlan where
  hashWithSalt _salt DeleteResponsePlan' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteResponsePlan where
  rnf DeleteResponsePlan' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteResponsePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResponsePlan where
  toJSON DeleteResponsePlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteResponsePlan where
  toPath = Prelude.const "/deleteResponsePlan"

instance Data.ToQuery DeleteResponsePlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResponsePlanResponse' smart constructor.
data DeleteResponsePlanResponse = DeleteResponsePlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResponsePlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResponsePlanResponse_httpStatus' - The response's http status code.
newDeleteResponsePlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResponsePlanResponse
newDeleteResponsePlanResponse pHttpStatus_ =
  DeleteResponsePlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResponsePlanResponse_httpStatus :: Lens.Lens' DeleteResponsePlanResponse Prelude.Int
deleteResponsePlanResponse_httpStatus = Lens.lens (\DeleteResponsePlanResponse' {httpStatus} -> httpStatus) (\s@DeleteResponsePlanResponse' {} a -> s {httpStatus = a} :: DeleteResponsePlanResponse)

instance Prelude.NFData DeleteResponsePlanResponse where
  rnf DeleteResponsePlanResponse' {..} =
    Prelude.rnf httpStatus
