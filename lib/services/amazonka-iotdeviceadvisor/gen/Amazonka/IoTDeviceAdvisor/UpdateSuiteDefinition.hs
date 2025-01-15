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
-- Module      : Amazonka.IoTDeviceAdvisor.UpdateSuiteDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Advisor test suite.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateSuiteDefinition>
-- action.
module Amazonka.IoTDeviceAdvisor.UpdateSuiteDefinition
  ( -- * Creating a Request
    UpdateSuiteDefinition (..),
    newUpdateSuiteDefinition,

    -- * Request Lenses
    updateSuiteDefinition_suiteDefinitionId,
    updateSuiteDefinition_suiteDefinitionConfiguration,

    -- * Destructuring the Response
    UpdateSuiteDefinitionResponse (..),
    newUpdateSuiteDefinitionResponse,

    -- * Response Lenses
    updateSuiteDefinitionResponse_createdAt,
    updateSuiteDefinitionResponse_lastUpdatedAt,
    updateSuiteDefinitionResponse_suiteDefinitionArn,
    updateSuiteDefinitionResponse_suiteDefinitionId,
    updateSuiteDefinitionResponse_suiteDefinitionName,
    updateSuiteDefinitionResponse_suiteDefinitionVersion,
    updateSuiteDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSuiteDefinition' smart constructor.
data UpdateSuiteDefinition = UpdateSuiteDefinition'
  { -- | Suite definition ID of the test suite to be updated.
    suiteDefinitionId :: Prelude.Text,
    -- | Updates a Device Advisor test suite with suite definition configuration.
    suiteDefinitionConfiguration :: SuiteDefinitionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSuiteDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suiteDefinitionId', 'updateSuiteDefinition_suiteDefinitionId' - Suite definition ID of the test suite to be updated.
--
-- 'suiteDefinitionConfiguration', 'updateSuiteDefinition_suiteDefinitionConfiguration' - Updates a Device Advisor test suite with suite definition configuration.
newUpdateSuiteDefinition ::
  -- | 'suiteDefinitionId'
  Prelude.Text ->
  -- | 'suiteDefinitionConfiguration'
  SuiteDefinitionConfiguration ->
  UpdateSuiteDefinition
newUpdateSuiteDefinition
  pSuiteDefinitionId_
  pSuiteDefinitionConfiguration_ =
    UpdateSuiteDefinition'
      { suiteDefinitionId =
          pSuiteDefinitionId_,
        suiteDefinitionConfiguration =
          pSuiteDefinitionConfiguration_
      }

-- | Suite definition ID of the test suite to be updated.
updateSuiteDefinition_suiteDefinitionId :: Lens.Lens' UpdateSuiteDefinition Prelude.Text
updateSuiteDefinition_suiteDefinitionId = Lens.lens (\UpdateSuiteDefinition' {suiteDefinitionId} -> suiteDefinitionId) (\s@UpdateSuiteDefinition' {} a -> s {suiteDefinitionId = a} :: UpdateSuiteDefinition)

-- | Updates a Device Advisor test suite with suite definition configuration.
updateSuiteDefinition_suiteDefinitionConfiguration :: Lens.Lens' UpdateSuiteDefinition SuiteDefinitionConfiguration
updateSuiteDefinition_suiteDefinitionConfiguration = Lens.lens (\UpdateSuiteDefinition' {suiteDefinitionConfiguration} -> suiteDefinitionConfiguration) (\s@UpdateSuiteDefinition' {} a -> s {suiteDefinitionConfiguration = a} :: UpdateSuiteDefinition)

instance Core.AWSRequest UpdateSuiteDefinition where
  type
    AWSResponse UpdateSuiteDefinition =
      UpdateSuiteDefinitionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSuiteDefinitionResponse'
            Prelude.<$> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "suiteDefinitionArn")
            Prelude.<*> (x Data..?> "suiteDefinitionId")
            Prelude.<*> (x Data..?> "suiteDefinitionName")
            Prelude.<*> (x Data..?> "suiteDefinitionVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSuiteDefinition where
  hashWithSalt _salt UpdateSuiteDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` suiteDefinitionId
      `Prelude.hashWithSalt` suiteDefinitionConfiguration

instance Prelude.NFData UpdateSuiteDefinition where
  rnf UpdateSuiteDefinition' {..} =
    Prelude.rnf suiteDefinitionId `Prelude.seq`
      Prelude.rnf suiteDefinitionConfiguration

instance Data.ToHeaders UpdateSuiteDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSuiteDefinition where
  toJSON UpdateSuiteDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "suiteDefinitionConfiguration"
                  Data..= suiteDefinitionConfiguration
              )
          ]
      )

instance Data.ToPath UpdateSuiteDefinition where
  toPath UpdateSuiteDefinition' {..} =
    Prelude.mconcat
      ["/suiteDefinitions/", Data.toBS suiteDefinitionId]

instance Data.ToQuery UpdateSuiteDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSuiteDefinitionResponse' smart constructor.
data UpdateSuiteDefinitionResponse = UpdateSuiteDefinitionResponse'
  { -- | Timestamp of when the test suite was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Timestamp of when the test suite was updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Amazon Resource Name (ARN) of the updated test suite.
    suiteDefinitionArn :: Prelude.Maybe Prelude.Text,
    -- | Suite definition ID of the updated test suite.
    suiteDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | Updates the suite definition name. This is a required parameter.
    suiteDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Suite definition version of the updated test suite.
    suiteDefinitionVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSuiteDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'updateSuiteDefinitionResponse_createdAt' - Timestamp of when the test suite was created.
--
-- 'lastUpdatedAt', 'updateSuiteDefinitionResponse_lastUpdatedAt' - Timestamp of when the test suite was updated.
--
-- 'suiteDefinitionArn', 'updateSuiteDefinitionResponse_suiteDefinitionArn' - Amazon Resource Name (ARN) of the updated test suite.
--
-- 'suiteDefinitionId', 'updateSuiteDefinitionResponse_suiteDefinitionId' - Suite definition ID of the updated test suite.
--
-- 'suiteDefinitionName', 'updateSuiteDefinitionResponse_suiteDefinitionName' - Updates the suite definition name. This is a required parameter.
--
-- 'suiteDefinitionVersion', 'updateSuiteDefinitionResponse_suiteDefinitionVersion' - Suite definition version of the updated test suite.
--
-- 'httpStatus', 'updateSuiteDefinitionResponse_httpStatus' - The response's http status code.
newUpdateSuiteDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSuiteDefinitionResponse
newUpdateSuiteDefinitionResponse pHttpStatus_ =
  UpdateSuiteDefinitionResponse'
    { createdAt =
        Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      suiteDefinitionArn = Prelude.Nothing,
      suiteDefinitionId = Prelude.Nothing,
      suiteDefinitionName = Prelude.Nothing,
      suiteDefinitionVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Timestamp of when the test suite was created.
updateSuiteDefinitionResponse_createdAt :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
updateSuiteDefinitionResponse_createdAt = Lens.lens (\UpdateSuiteDefinitionResponse' {createdAt} -> createdAt) (\s@UpdateSuiteDefinitionResponse' {} a -> s {createdAt = a} :: UpdateSuiteDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | Timestamp of when the test suite was updated.
updateSuiteDefinitionResponse_lastUpdatedAt :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
updateSuiteDefinitionResponse_lastUpdatedAt = Lens.lens (\UpdateSuiteDefinitionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@UpdateSuiteDefinitionResponse' {} a -> s {lastUpdatedAt = a} :: UpdateSuiteDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | Amazon Resource Name (ARN) of the updated test suite.
updateSuiteDefinitionResponse_suiteDefinitionArn :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionArn = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionArn} -> suiteDefinitionArn) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionArn = a} :: UpdateSuiteDefinitionResponse)

-- | Suite definition ID of the updated test suite.
updateSuiteDefinitionResponse_suiteDefinitionId :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionId = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionId} -> suiteDefinitionId) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionId = a} :: UpdateSuiteDefinitionResponse)

-- | Updates the suite definition name. This is a required parameter.
updateSuiteDefinitionResponse_suiteDefinitionName :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionName = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionName} -> suiteDefinitionName) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionName = a} :: UpdateSuiteDefinitionResponse)

-- | Suite definition version of the updated test suite.
updateSuiteDefinitionResponse_suiteDefinitionVersion :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionVersion = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionVersion} -> suiteDefinitionVersion) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionVersion = a} :: UpdateSuiteDefinitionResponse)

-- | The response's http status code.
updateSuiteDefinitionResponse_httpStatus :: Lens.Lens' UpdateSuiteDefinitionResponse Prelude.Int
updateSuiteDefinitionResponse_httpStatus = Lens.lens (\UpdateSuiteDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateSuiteDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateSuiteDefinitionResponse)

instance Prelude.NFData UpdateSuiteDefinitionResponse where
  rnf UpdateSuiteDefinitionResponse' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf lastUpdatedAt `Prelude.seq`
        Prelude.rnf suiteDefinitionArn `Prelude.seq`
          Prelude.rnf suiteDefinitionId `Prelude.seq`
            Prelude.rnf suiteDefinitionName `Prelude.seq`
              Prelude.rnf suiteDefinitionVersion `Prelude.seq`
                Prelude.rnf httpStatus
