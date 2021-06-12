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
-- Module      : Network.AWS.DynamoDB.DescribeContinuousBackups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the status of continuous backups and point in time recovery on
-- the specified table. Continuous backups are @ENABLED@ on all tables at
-- table creation. If point in time recovery is enabled,
-- @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
-- After continuous backups and point in time recovery are enabled, you can
-- restore to any point in time within @EarliestRestorableDateTime@ and
-- @LatestRestorableDateTime@.
--
-- @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time. You can restore your table to any point in time during the last 35
-- days.
--
-- You can call @DescribeContinuousBackups@ at a maximum rate of 10 times
-- per second.
module Network.AWS.DynamoDB.DescribeContinuousBackups
  ( -- * Creating a Request
    DescribeContinuousBackups (..),
    newDescribeContinuousBackups,

    -- * Request Lenses
    describeContinuousBackups_tableName,

    -- * Destructuring the Response
    DescribeContinuousBackupsResponse (..),
    newDescribeContinuousBackupsResponse,

    -- * Response Lenses
    describeContinuousBackupsResponse_continuousBackupsDescription,
    describeContinuousBackupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeContinuousBackups' smart constructor.
data DescribeContinuousBackups = DescribeContinuousBackups'
  { -- | Name of the table for which the customer wants to check the continuous
    -- backups and point in time recovery settings.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeContinuousBackups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'describeContinuousBackups_tableName' - Name of the table for which the customer wants to check the continuous
-- backups and point in time recovery settings.
newDescribeContinuousBackups ::
  -- | 'tableName'
  Core.Text ->
  DescribeContinuousBackups
newDescribeContinuousBackups pTableName_ =
  DescribeContinuousBackups' {tableName = pTableName_}

-- | Name of the table for which the customer wants to check the continuous
-- backups and point in time recovery settings.
describeContinuousBackups_tableName :: Lens.Lens' DescribeContinuousBackups Core.Text
describeContinuousBackups_tableName = Lens.lens (\DescribeContinuousBackups' {tableName} -> tableName) (\s@DescribeContinuousBackups' {} a -> s {tableName = a} :: DescribeContinuousBackups)

instance Core.AWSRequest DescribeContinuousBackups where
  type
    AWSResponse DescribeContinuousBackups =
      DescribeContinuousBackupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContinuousBackupsResponse'
            Core.<$> (x Core..?> "ContinuousBackupsDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeContinuousBackups

instance Core.NFData DescribeContinuousBackups

instance Core.ToHeaders DescribeContinuousBackups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DescribeContinuousBackups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeContinuousBackups where
  toJSON DescribeContinuousBackups' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TableName" Core..= tableName)]
      )

instance Core.ToPath DescribeContinuousBackups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeContinuousBackups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeContinuousBackupsResponse' smart constructor.
data DescribeContinuousBackupsResponse = DescribeContinuousBackupsResponse'
  { -- | Represents the continuous backups and point in time recovery settings on
    -- the table.
    continuousBackupsDescription :: Core.Maybe ContinuousBackupsDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeContinuousBackupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousBackupsDescription', 'describeContinuousBackupsResponse_continuousBackupsDescription' - Represents the continuous backups and point in time recovery settings on
-- the table.
--
-- 'httpStatus', 'describeContinuousBackupsResponse_httpStatus' - The response's http status code.
newDescribeContinuousBackupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeContinuousBackupsResponse
newDescribeContinuousBackupsResponse pHttpStatus_ =
  DescribeContinuousBackupsResponse'
    { continuousBackupsDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the continuous backups and point in time recovery settings on
-- the table.
describeContinuousBackupsResponse_continuousBackupsDescription :: Lens.Lens' DescribeContinuousBackupsResponse (Core.Maybe ContinuousBackupsDescription)
describeContinuousBackupsResponse_continuousBackupsDescription = Lens.lens (\DescribeContinuousBackupsResponse' {continuousBackupsDescription} -> continuousBackupsDescription) (\s@DescribeContinuousBackupsResponse' {} a -> s {continuousBackupsDescription = a} :: DescribeContinuousBackupsResponse)

-- | The response's http status code.
describeContinuousBackupsResponse_httpStatus :: Lens.Lens' DescribeContinuousBackupsResponse Core.Int
describeContinuousBackupsResponse_httpStatus = Lens.lens (\DescribeContinuousBackupsResponse' {httpStatus} -> httpStatus) (\s@DescribeContinuousBackupsResponse' {} a -> s {httpStatus = a} :: DescribeContinuousBackupsResponse)

instance
  Core.NFData
    DescribeContinuousBackupsResponse
