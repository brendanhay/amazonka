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
-- Module      : Network.AWS.DynamoDB.UpdateContinuousBackups
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.DynamoDB.UpdateContinuousBackups
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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContinuousBackups' smart constructor.
data UpdateContinuousBackups = UpdateContinuousBackups'
  { -- | The name of the table.
    tableName :: Core.Text,
    -- | Represents the settings used to enable point in time recovery.
    pointInTimeRecoverySpecification :: PointInTimeRecoverySpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
updateContinuousBackups_tableName :: Lens.Lens' UpdateContinuousBackups Core.Text
updateContinuousBackups_tableName = Lens.lens (\UpdateContinuousBackups' {tableName} -> tableName) (\s@UpdateContinuousBackups' {} a -> s {tableName = a} :: UpdateContinuousBackups)

-- | Represents the settings used to enable point in time recovery.
updateContinuousBackups_pointInTimeRecoverySpecification :: Lens.Lens' UpdateContinuousBackups PointInTimeRecoverySpecification
updateContinuousBackups_pointInTimeRecoverySpecification = Lens.lens (\UpdateContinuousBackups' {pointInTimeRecoverySpecification} -> pointInTimeRecoverySpecification) (\s@UpdateContinuousBackups' {} a -> s {pointInTimeRecoverySpecification = a} :: UpdateContinuousBackups)

instance Core.AWSRequest UpdateContinuousBackups where
  type
    AWSResponse UpdateContinuousBackups =
      UpdateContinuousBackupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContinuousBackupsResponse'
            Core.<$> (x Core..?> "ContinuousBackupsDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateContinuousBackups

instance Core.NFData UpdateContinuousBackups

instance Core.ToHeaders UpdateContinuousBackups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.UpdateContinuousBackups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateContinuousBackups where
  toJSON UpdateContinuousBackups' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            Core.Just
              ( "PointInTimeRecoverySpecification"
                  Core..= pointInTimeRecoverySpecification
              )
          ]
      )

instance Core.ToPath UpdateContinuousBackups where
  toPath = Core.const "/"

instance Core.ToQuery UpdateContinuousBackups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateContinuousBackupsResponse' smart constructor.
data UpdateContinuousBackupsResponse = UpdateContinuousBackupsResponse'
  { -- | Represents the continuous backups and point in time recovery settings on
    -- the table.
    continuousBackupsDescription :: Core.Maybe ContinuousBackupsDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateContinuousBackupsResponse
newUpdateContinuousBackupsResponse pHttpStatus_ =
  UpdateContinuousBackupsResponse'
    { continuousBackupsDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the continuous backups and point in time recovery settings on
-- the table.
updateContinuousBackupsResponse_continuousBackupsDescription :: Lens.Lens' UpdateContinuousBackupsResponse (Core.Maybe ContinuousBackupsDescription)
updateContinuousBackupsResponse_continuousBackupsDescription = Lens.lens (\UpdateContinuousBackupsResponse' {continuousBackupsDescription} -> continuousBackupsDescription) (\s@UpdateContinuousBackupsResponse' {} a -> s {continuousBackupsDescription = a} :: UpdateContinuousBackupsResponse)

-- | The response's http status code.
updateContinuousBackupsResponse_httpStatus :: Lens.Lens' UpdateContinuousBackupsResponse Core.Int
updateContinuousBackupsResponse_httpStatus = Lens.lens (\UpdateContinuousBackupsResponse' {httpStatus} -> httpStatus) (\s@UpdateContinuousBackupsResponse' {} a -> s {httpStatus = a} :: UpdateContinuousBackupsResponse)

instance Core.NFData UpdateContinuousBackupsResponse
