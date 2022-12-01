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
-- Module      : Amazonka.IoTDeviceAdvisor.DeleteSuiteDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Advisor test suite.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteSuiteDefinition>
-- action.
module Amazonka.IoTDeviceAdvisor.DeleteSuiteDefinition
  ( -- * Creating a Request
    DeleteSuiteDefinition (..),
    newDeleteSuiteDefinition,

    -- * Request Lenses
    deleteSuiteDefinition_suiteDefinitionId,

    -- * Destructuring the Response
    DeleteSuiteDefinitionResponse (..),
    newDeleteSuiteDefinitionResponse,

    -- * Response Lenses
    deleteSuiteDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTDeviceAdvisor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSuiteDefinition' smart constructor.
data DeleteSuiteDefinition = DeleteSuiteDefinition'
  { -- | Suite definition ID of the test suite to be deleted.
    suiteDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSuiteDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suiteDefinitionId', 'deleteSuiteDefinition_suiteDefinitionId' - Suite definition ID of the test suite to be deleted.
newDeleteSuiteDefinition ::
  -- | 'suiteDefinitionId'
  Prelude.Text ->
  DeleteSuiteDefinition
newDeleteSuiteDefinition pSuiteDefinitionId_ =
  DeleteSuiteDefinition'
    { suiteDefinitionId =
        pSuiteDefinitionId_
    }

-- | Suite definition ID of the test suite to be deleted.
deleteSuiteDefinition_suiteDefinitionId :: Lens.Lens' DeleteSuiteDefinition Prelude.Text
deleteSuiteDefinition_suiteDefinitionId = Lens.lens (\DeleteSuiteDefinition' {suiteDefinitionId} -> suiteDefinitionId) (\s@DeleteSuiteDefinition' {} a -> s {suiteDefinitionId = a} :: DeleteSuiteDefinition)

instance Core.AWSRequest DeleteSuiteDefinition where
  type
    AWSResponse DeleteSuiteDefinition =
      DeleteSuiteDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSuiteDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSuiteDefinition where
  hashWithSalt _salt DeleteSuiteDefinition' {..} =
    _salt `Prelude.hashWithSalt` suiteDefinitionId

instance Prelude.NFData DeleteSuiteDefinition where
  rnf DeleteSuiteDefinition' {..} =
    Prelude.rnf suiteDefinitionId

instance Core.ToHeaders DeleteSuiteDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteSuiteDefinition where
  toPath DeleteSuiteDefinition' {..} =
    Prelude.mconcat
      ["/suiteDefinitions/", Core.toBS suiteDefinitionId]

instance Core.ToQuery DeleteSuiteDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSuiteDefinitionResponse' smart constructor.
data DeleteSuiteDefinitionResponse = DeleteSuiteDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSuiteDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSuiteDefinitionResponse_httpStatus' - The response's http status code.
newDeleteSuiteDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSuiteDefinitionResponse
newDeleteSuiteDefinitionResponse pHttpStatus_ =
  DeleteSuiteDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSuiteDefinitionResponse_httpStatus :: Lens.Lens' DeleteSuiteDefinitionResponse Prelude.Int
deleteSuiteDefinitionResponse_httpStatus = Lens.lens (\DeleteSuiteDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteSuiteDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteSuiteDefinitionResponse)

instance Prelude.NFData DeleteSuiteDefinitionResponse where
  rnf DeleteSuiteDefinitionResponse' {..} =
    Prelude.rnf httpStatus
