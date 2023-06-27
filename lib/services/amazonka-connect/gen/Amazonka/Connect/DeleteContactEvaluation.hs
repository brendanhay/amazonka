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
-- Module      : Amazonka.Connect.DeleteContactEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact evaluation in the specified Amazon Connect instance.
module Amazonka.Connect.DeleteContactEvaluation
  ( -- * Creating a Request
    DeleteContactEvaluation (..),
    newDeleteContactEvaluation,

    -- * Request Lenses
    deleteContactEvaluation_instanceId,
    deleteContactEvaluation_evaluationId,

    -- * Destructuring the Response
    DeleteContactEvaluationResponse (..),
    newDeleteContactEvaluationResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContactEvaluation' smart constructor.
data DeleteContactEvaluation = DeleteContactEvaluation'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the contact evaluation.
    evaluationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteContactEvaluation_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationId', 'deleteContactEvaluation_evaluationId' - A unique identifier for the contact evaluation.
newDeleteContactEvaluation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationId'
  Prelude.Text ->
  DeleteContactEvaluation
newDeleteContactEvaluation
  pInstanceId_
  pEvaluationId_ =
    DeleteContactEvaluation'
      { instanceId = pInstanceId_,
        evaluationId = pEvaluationId_
      }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
deleteContactEvaluation_instanceId :: Lens.Lens' DeleteContactEvaluation Prelude.Text
deleteContactEvaluation_instanceId = Lens.lens (\DeleteContactEvaluation' {instanceId} -> instanceId) (\s@DeleteContactEvaluation' {} a -> s {instanceId = a} :: DeleteContactEvaluation)

-- | A unique identifier for the contact evaluation.
deleteContactEvaluation_evaluationId :: Lens.Lens' DeleteContactEvaluation Prelude.Text
deleteContactEvaluation_evaluationId = Lens.lens (\DeleteContactEvaluation' {evaluationId} -> evaluationId) (\s@DeleteContactEvaluation' {} a -> s {evaluationId = a} :: DeleteContactEvaluation)

instance Core.AWSRequest DeleteContactEvaluation where
  type
    AWSResponse DeleteContactEvaluation =
      DeleteContactEvaluationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteContactEvaluationResponse'

instance Prelude.Hashable DeleteContactEvaluation where
  hashWithSalt _salt DeleteContactEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationId

instance Prelude.NFData DeleteContactEvaluation where
  rnf DeleteContactEvaluation' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationId

instance Data.ToHeaders DeleteContactEvaluation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteContactEvaluation where
  toPath DeleteContactEvaluation' {..} =
    Prelude.mconcat
      [ "/contact-evaluations/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationId
      ]

instance Data.ToQuery DeleteContactEvaluation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactEvaluationResponse' smart constructor.
data DeleteContactEvaluationResponse = DeleteContactEvaluationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactEvaluationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteContactEvaluationResponse ::
  DeleteContactEvaluationResponse
newDeleteContactEvaluationResponse =
  DeleteContactEvaluationResponse'

instance
  Prelude.NFData
    DeleteContactEvaluationResponse
  where
  rnf _ = ()
