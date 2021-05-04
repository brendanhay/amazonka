{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeContinuousBackups' smart constructor.
data DescribeContinuousBackups = DescribeContinuousBackups'
  { -- | Name of the table for which the customer wants to check the continuous
    -- backups and point in time recovery settings.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeContinuousBackups
newDescribeContinuousBackups pTableName_ =
  DescribeContinuousBackups' {tableName = pTableName_}

-- | Name of the table for which the customer wants to check the continuous
-- backups and point in time recovery settings.
describeContinuousBackups_tableName :: Lens.Lens' DescribeContinuousBackups Prelude.Text
describeContinuousBackups_tableName = Lens.lens (\DescribeContinuousBackups' {tableName} -> tableName) (\s@DescribeContinuousBackups' {} a -> s {tableName = a} :: DescribeContinuousBackups)

instance Prelude.AWSRequest DescribeContinuousBackups where
  type
    Rs DescribeContinuousBackups =
      DescribeContinuousBackupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContinuousBackupsResponse'
            Prelude.<$> (x Prelude..?> "ContinuousBackupsDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContinuousBackups

instance Prelude.NFData DescribeContinuousBackups

instance Prelude.ToHeaders DescribeContinuousBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.DescribeContinuousBackups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeContinuousBackups where
  toJSON DescribeContinuousBackups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Prelude..= tableName)]
      )

instance Prelude.ToPath DescribeContinuousBackups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeContinuousBackups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContinuousBackupsResponse' smart constructor.
data DescribeContinuousBackupsResponse = DescribeContinuousBackupsResponse'
  { -- | Represents the continuous backups and point in time recovery settings on
    -- the table.
    continuousBackupsDescription :: Prelude.Maybe ContinuousBackupsDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeContinuousBackupsResponse
newDescribeContinuousBackupsResponse pHttpStatus_ =
  DescribeContinuousBackupsResponse'
    { continuousBackupsDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the continuous backups and point in time recovery settings on
-- the table.
describeContinuousBackupsResponse_continuousBackupsDescription :: Lens.Lens' DescribeContinuousBackupsResponse (Prelude.Maybe ContinuousBackupsDescription)
describeContinuousBackupsResponse_continuousBackupsDescription = Lens.lens (\DescribeContinuousBackupsResponse' {continuousBackupsDescription} -> continuousBackupsDescription) (\s@DescribeContinuousBackupsResponse' {} a -> s {continuousBackupsDescription = a} :: DescribeContinuousBackupsResponse)

-- | The response's http status code.
describeContinuousBackupsResponse_httpStatus :: Lens.Lens' DescribeContinuousBackupsResponse Prelude.Int
describeContinuousBackupsResponse_httpStatus = Lens.lens (\DescribeContinuousBackupsResponse' {httpStatus} -> httpStatus) (\s@DescribeContinuousBackupsResponse' {} a -> s {httpStatus = a} :: DescribeContinuousBackupsResponse)

instance
  Prelude.NFData
    DescribeContinuousBackupsResponse
