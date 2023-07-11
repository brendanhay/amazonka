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
-- Module      : Amazonka.FraudDetector.DeleteVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a variable.
--
-- You can\'t delete variables that are included in an event type in Amazon
-- Fraud Detector.
--
-- Amazon Fraud Detector automatically deletes model output variables and
-- SageMaker model output variables when you delete the model. You can\'t
-- delete these variables manually.
--
-- When you delete a variable, Amazon Fraud Detector permanently deletes
-- that variable and the data is no longer stored in Amazon Fraud Detector.
module Amazonka.FraudDetector.DeleteVariable
  ( -- * Creating a Request
    DeleteVariable (..),
    newDeleteVariable,

    -- * Request Lenses
    deleteVariable_name,

    -- * Destructuring the Response
    DeleteVariableResponse (..),
    newDeleteVariableResponse,

    -- * Response Lenses
    deleteVariableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVariable' smart constructor.
data DeleteVariable = DeleteVariable'
  { -- | The name of the variable to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteVariable_name' - The name of the variable to delete.
newDeleteVariable ::
  -- | 'name'
  Prelude.Text ->
  DeleteVariable
newDeleteVariable pName_ =
  DeleteVariable' {name = pName_}

-- | The name of the variable to delete.
deleteVariable_name :: Lens.Lens' DeleteVariable Prelude.Text
deleteVariable_name = Lens.lens (\DeleteVariable' {name} -> name) (\s@DeleteVariable' {} a -> s {name = a} :: DeleteVariable)

instance Core.AWSRequest DeleteVariable where
  type
    AWSResponse DeleteVariable =
      DeleteVariableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVariableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVariable where
  hashWithSalt _salt DeleteVariable' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteVariable where
  rnf DeleteVariable' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteVariable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DeleteVariable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteVariable where
  toJSON DeleteVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath DeleteVariable where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVariable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVariableResponse' smart constructor.
data DeleteVariableResponse = DeleteVariableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVariableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVariableResponse_httpStatus' - The response's http status code.
newDeleteVariableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVariableResponse
newDeleteVariableResponse pHttpStatus_ =
  DeleteVariableResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteVariableResponse_httpStatus :: Lens.Lens' DeleteVariableResponse Prelude.Int
deleteVariableResponse_httpStatus = Lens.lens (\DeleteVariableResponse' {httpStatus} -> httpStatus) (\s@DeleteVariableResponse' {} a -> s {httpStatus = a} :: DeleteVariableResponse)

instance Prelude.NFData DeleteVariableResponse where
  rnf DeleteVariableResponse' {..} =
    Prelude.rnf httpStatus
