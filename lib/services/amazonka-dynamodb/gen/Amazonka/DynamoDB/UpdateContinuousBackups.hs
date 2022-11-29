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
-- Module      : Amazonka.DynamoDB.UpdateContinuousBackups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @UpdateContinuousBackups@ enables or disables point in time recovery for
-- the specified table. A successful @UpdateContinuousBackups@ call returns
-- the current @ContinuousBackupsDescription@. Continuous backups are
-- @ENABLED@ on all tables at table creation. If point in time recovery is
-- enabled, @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
-- Once continuous backups and point in time recovery are enabled, you can
-- restore to any point in time within @EarliestRestorableDateTime@ and
-- @LatestRestorableDateTime@.
--
-- @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time. You can restore your table to any point in time during the last 35
-- days.
module Amazonka.DynamoDB.UpdateContinuousBackups
  ( -- * Creating a Request
    UpdateContinuousBackups (..),
    newUpdateContinuousBackups,

    -- * Request Lenses
    updateContinuousBackups_tableName,
    updateContinuousBackups_pointInTimeRecoverySpecification,

    -- * Destructuring the Response
    UpdateContinuousBackupsResponse (..),
    newUpdateContinuousBackupsResponse,

    -- * Response Lenses
    updateContinuousBackupsResponse_continuousBackupsDescription,
    updateContinuousBackupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContinuousBackups' smart constructor.
data UpdateContinuousBackups = UpdateContinuousBackups'
  { -- | The name of the table.
    tableName :: Prelude.Text,
    -- | Represents the settings used to enable point in time recovery.
    pointInTimeRecoverySpecification :: PointInTimeRecoverySpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContinuousBackups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'updateContinuousBackups_tableName' - The name of the table.
--
-- 'pointInTimeRecoverySpecification', 'updateContinuousBackups_pointInTimeRecoverySpecification' - Represents the settings used to enable point in time recovery.
newUpdateContinuousBackups ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'pointInTimeRecoverySpecification'
  PointInTimeRecoverySpecification ->
  UpdateContinuousBackups
newUpdateContinuousBackups
  pTableName_
  pPointInTimeRecoverySpecification_ =
    UpdateContinuousBackups'
      { tableName = pTableName_,
        pointInTimeRecoverySpecification =
          pPointInTimeRecoverySpecification_
      }

-- | The name of the table.
updateContinuousBackups_tableName :: Lens.Lens' UpdateContinuousBackups Prelude.Text
updateContinuousBackups_tableName = Lens.lens (\UpdateContinuousBackups' {tableName} -> tableName) (\s@UpdateContinuousBackups' {} a -> s {tableName = a} :: UpdateContinuousBackups)

-- | Represents the settings used to enable point in time recovery.
updateContinuousBackups_pointInTimeRecoverySpecification :: Lens.Lens' UpdateContinuousBackups PointInTimeRecoverySpecification
updateContinuousBackups_pointInTimeRecoverySpecification = Lens.lens (\UpdateContinuousBackups' {pointInTimeRecoverySpecification} -> pointInTimeRecoverySpecification) (\s@UpdateContinuousBackups' {} a -> s {pointInTimeRecoverySpecification = a} :: UpdateContinuousBackups)

instance Core.AWSRequest UpdateContinuousBackups where
  type
    AWSResponse UpdateContinuousBackups =
      UpdateContinuousBackupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContinuousBackupsResponse'
            Prelude.<$> (x Core..?> "ContinuousBackupsDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContinuousBackups where
  hashWithSalt _salt UpdateContinuousBackups' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` pointInTimeRecoverySpecification

instance Prelude.NFData UpdateContinuousBackups where
  rnf UpdateContinuousBackups' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf pointInTimeRecoverySpecification

instance Core.ToHeaders UpdateContinuousBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.UpdateContinuousBackups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateContinuousBackups where
  toJSON UpdateContinuousBackups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just
              ( "PointInTimeRecoverySpecification"
                  Core..= pointInTimeRecoverySpecification
              )
          ]
      )

instance Core.ToPath UpdateContinuousBackups where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateContinuousBackups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContinuousBackupsResponse' smart constructor.
data UpdateContinuousBackupsResponse = UpdateContinuousBackupsResponse'
  { -- | Represents the continuous backups and point in time recovery settings on
    -- the table.
    continuousBackupsDescription :: Prelude.Maybe ContinuousBackupsDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContinuousBackupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousBackupsDescription', 'updateContinuousBackupsResponse_continuousBackupsDescription' - Represents the continuous backups and point in time recovery settings on
-- the table.
--
-- 'httpStatus', 'updateContinuousBackupsResponse_httpStatus' - The response's http status code.
newUpdateContinuousBackupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContinuousBackupsResponse
newUpdateContinuousBackupsResponse pHttpStatus_ =
  UpdateContinuousBackupsResponse'
    { continuousBackupsDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the continuous backups and point in time recovery settings on
-- the table.
updateContinuousBackupsResponse_continuousBackupsDescription :: Lens.Lens' UpdateContinuousBackupsResponse (Prelude.Maybe ContinuousBackupsDescription)
updateContinuousBackupsResponse_continuousBackupsDescription = Lens.lens (\UpdateContinuousBackupsResponse' {continuousBackupsDescription} -> continuousBackupsDescription) (\s@UpdateContinuousBackupsResponse' {} a -> s {continuousBackupsDescription = a} :: UpdateContinuousBackupsResponse)

-- | The response's http status code.
updateContinuousBackupsResponse_httpStatus :: Lens.Lens' UpdateContinuousBackupsResponse Prelude.Int
updateContinuousBackupsResponse_httpStatus = Lens.lens (\UpdateContinuousBackupsResponse' {httpStatus} -> httpStatus) (\s@UpdateContinuousBackupsResponse' {} a -> s {httpStatus = a} :: UpdateContinuousBackupsResponse)

instance
  Prelude.NFData
    UpdateContinuousBackupsResponse
  where
  rnf UpdateContinuousBackupsResponse' {..} =
    Prelude.rnf continuousBackupsDescription
      `Prelude.seq` Prelude.rnf httpStatus
