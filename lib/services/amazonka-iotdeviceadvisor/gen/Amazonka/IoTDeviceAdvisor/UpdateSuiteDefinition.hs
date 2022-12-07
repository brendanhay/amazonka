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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    updateSuiteDefinition_suiteDefinitionConfiguration,
    updateSuiteDefinition_suiteDefinitionId,

    -- * Destructuring the Response
    UpdateSuiteDefinitionResponse (..),
    newUpdateSuiteDefinitionResponse,

    -- * Response Lenses
    updateSuiteDefinitionResponse_lastUpdatedAt,
    updateSuiteDefinitionResponse_suiteDefinitionName,
    updateSuiteDefinitionResponse_suiteDefinitionArn,
    updateSuiteDefinitionResponse_suiteDefinitionVersion,
    updateSuiteDefinitionResponse_suiteDefinitionId,
    updateSuiteDefinitionResponse_createdAt,
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
  { -- | Updates a Device Advisor test suite with suite definition configuration.
    suiteDefinitionConfiguration :: Prelude.Maybe SuiteDefinitionConfiguration,
    -- | Suite definition ID of the test suite to be updated.
    suiteDefinitionId :: Prelude.Text
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
-- 'suiteDefinitionConfiguration', 'updateSuiteDefinition_suiteDefinitionConfiguration' - Updates a Device Advisor test suite with suite definition configuration.
--
-- 'suiteDefinitionId', 'updateSuiteDefinition_suiteDefinitionId' - Suite definition ID of the test suite to be updated.
newUpdateSuiteDefinition ::
  -- | 'suiteDefinitionId'
  Prelude.Text ->
  UpdateSuiteDefinition
newUpdateSuiteDefinition pSuiteDefinitionId_ =
  UpdateSuiteDefinition'
    { suiteDefinitionConfiguration =
        Prelude.Nothing,
      suiteDefinitionId = pSuiteDefinitionId_
    }

-- | Updates a Device Advisor test suite with suite definition configuration.
updateSuiteDefinition_suiteDefinitionConfiguration :: Lens.Lens' UpdateSuiteDefinition (Prelude.Maybe SuiteDefinitionConfiguration)
updateSuiteDefinition_suiteDefinitionConfiguration = Lens.lens (\UpdateSuiteDefinition' {suiteDefinitionConfiguration} -> suiteDefinitionConfiguration) (\s@UpdateSuiteDefinition' {} a -> s {suiteDefinitionConfiguration = a} :: UpdateSuiteDefinition)

-- | Suite definition ID of the test suite to be updated.
updateSuiteDefinition_suiteDefinitionId :: Lens.Lens' UpdateSuiteDefinition Prelude.Text
updateSuiteDefinition_suiteDefinitionId = Lens.lens (\UpdateSuiteDefinition' {suiteDefinitionId} -> suiteDefinitionId) (\s@UpdateSuiteDefinition' {} a -> s {suiteDefinitionId = a} :: UpdateSuiteDefinition)

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
            Prelude.<$> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "suiteDefinitionName")
            Prelude.<*> (x Data..?> "suiteDefinitionArn")
            Prelude.<*> (x Data..?> "suiteDefinitionVersion")
            Prelude.<*> (x Data..?> "suiteDefinitionId")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSuiteDefinition where
  hashWithSalt _salt UpdateSuiteDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` suiteDefinitionConfiguration
      `Prelude.hashWithSalt` suiteDefinitionId

instance Prelude.NFData UpdateSuiteDefinition where
  rnf UpdateSuiteDefinition' {..} =
    Prelude.rnf suiteDefinitionConfiguration
      `Prelude.seq` Prelude.rnf suiteDefinitionId

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
          [ ("suiteDefinitionConfiguration" Data..=)
              Prelude.<$> suiteDefinitionConfiguration
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
  { -- | Timestamp of when the test suite was updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Suite definition name of the updated test suite.
    suiteDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the updated test suite.
    suiteDefinitionArn :: Prelude.Maybe Prelude.Text,
    -- | Suite definition version of the updated test suite.
    suiteDefinitionVersion :: Prelude.Maybe Prelude.Text,
    -- | Suite definition ID of the updated test suite.
    suiteDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the test suite was created.
    createdAt :: Prelude.Maybe Data.POSIX,
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
-- 'lastUpdatedAt', 'updateSuiteDefinitionResponse_lastUpdatedAt' - Timestamp of when the test suite was updated.
--
-- 'suiteDefinitionName', 'updateSuiteDefinitionResponse_suiteDefinitionName' - Suite definition name of the updated test suite.
--
-- 'suiteDefinitionArn', 'updateSuiteDefinitionResponse_suiteDefinitionArn' - Amazon Resource Name (ARN) of the updated test suite.
--
-- 'suiteDefinitionVersion', 'updateSuiteDefinitionResponse_suiteDefinitionVersion' - Suite definition version of the updated test suite.
--
-- 'suiteDefinitionId', 'updateSuiteDefinitionResponse_suiteDefinitionId' - Suite definition ID of the updated test suite.
--
-- 'createdAt', 'updateSuiteDefinitionResponse_createdAt' - Timestamp of when the test suite was created.
--
-- 'httpStatus', 'updateSuiteDefinitionResponse_httpStatus' - The response's http status code.
newUpdateSuiteDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSuiteDefinitionResponse
newUpdateSuiteDefinitionResponse pHttpStatus_ =
  UpdateSuiteDefinitionResponse'
    { lastUpdatedAt =
        Prelude.Nothing,
      suiteDefinitionName = Prelude.Nothing,
      suiteDefinitionArn = Prelude.Nothing,
      suiteDefinitionVersion = Prelude.Nothing,
      suiteDefinitionId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Timestamp of when the test suite was updated.
updateSuiteDefinitionResponse_lastUpdatedAt :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
updateSuiteDefinitionResponse_lastUpdatedAt = Lens.lens (\UpdateSuiteDefinitionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@UpdateSuiteDefinitionResponse' {} a -> s {lastUpdatedAt = a} :: UpdateSuiteDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | Suite definition name of the updated test suite.
updateSuiteDefinitionResponse_suiteDefinitionName :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionName = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionName} -> suiteDefinitionName) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionName = a} :: UpdateSuiteDefinitionResponse)

-- | Amazon Resource Name (ARN) of the updated test suite.
updateSuiteDefinitionResponse_suiteDefinitionArn :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionArn = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionArn} -> suiteDefinitionArn) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionArn = a} :: UpdateSuiteDefinitionResponse)

-- | Suite definition version of the updated test suite.
updateSuiteDefinitionResponse_suiteDefinitionVersion :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionVersion = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionVersion} -> suiteDefinitionVersion) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionVersion = a} :: UpdateSuiteDefinitionResponse)

-- | Suite definition ID of the updated test suite.
updateSuiteDefinitionResponse_suiteDefinitionId :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.Text)
updateSuiteDefinitionResponse_suiteDefinitionId = Lens.lens (\UpdateSuiteDefinitionResponse' {suiteDefinitionId} -> suiteDefinitionId) (\s@UpdateSuiteDefinitionResponse' {} a -> s {suiteDefinitionId = a} :: UpdateSuiteDefinitionResponse)

-- | Timestamp of when the test suite was created.
updateSuiteDefinitionResponse_createdAt :: Lens.Lens' UpdateSuiteDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
updateSuiteDefinitionResponse_createdAt = Lens.lens (\UpdateSuiteDefinitionResponse' {createdAt} -> createdAt) (\s@UpdateSuiteDefinitionResponse' {} a -> s {createdAt = a} :: UpdateSuiteDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateSuiteDefinitionResponse_httpStatus :: Lens.Lens' UpdateSuiteDefinitionResponse Prelude.Int
updateSuiteDefinitionResponse_httpStatus = Lens.lens (\UpdateSuiteDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateSuiteDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateSuiteDefinitionResponse)

instance Prelude.NFData UpdateSuiteDefinitionResponse where
  rnf UpdateSuiteDefinitionResponse' {..} =
    Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf suiteDefinitionName
      `Prelude.seq` Prelude.rnf suiteDefinitionArn
      `Prelude.seq` Prelude.rnf suiteDefinitionVersion
      `Prelude.seq` Prelude.rnf suiteDefinitionId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus
