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
-- Module      : Amazonka.DynamoDB.DescribeContinuousBackups
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.DynamoDB.DescribeContinuousBackups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeContinuousBackups' smart constructor.
data DescribeContinuousBackups = DescribeContinuousBackups'
  { -- | Name of the table for which the customer wants to check the continuous
    -- backups and point in time recovery settings.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeContinuousBackups where
  type
    AWSResponse DescribeContinuousBackups =
      DescribeContinuousBackupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContinuousBackupsResponse'
            Prelude.<$> (x Data..?> "ContinuousBackupsDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContinuousBackups where
  hashWithSalt _salt DescribeContinuousBackups' {..} =
    _salt `Prelude.hashWithSalt` tableName

instance Prelude.NFData DescribeContinuousBackups where
  rnf DescribeContinuousBackups' {..} =
    Prelude.rnf tableName

instance Data.ToHeaders DescribeContinuousBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeContinuousBackups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeContinuousBackups where
  toJSON DescribeContinuousBackups' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Data..= tableName)]
      )

instance Data.ToPath DescribeContinuousBackups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeContinuousBackups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContinuousBackupsResponse' smart constructor.
data DescribeContinuousBackupsResponse = DescribeContinuousBackupsResponse'
  { -- | Represents the continuous backups and point in time recovery settings on
    -- the table.
    continuousBackupsDescription :: Prelude.Maybe ContinuousBackupsDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DescribeContinuousBackupsResponse' {..} =
    Prelude.rnf continuousBackupsDescription
      `Prelude.seq` Prelude.rnf httpStatus
