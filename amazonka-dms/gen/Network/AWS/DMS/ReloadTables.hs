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
-- Module      : Network.AWS.DMS.ReloadTables
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reloads the target database table with the source data.
module Network.AWS.DMS.ReloadTables
  ( -- * Creating a Request
    ReloadTables (..),
    newReloadTables,

    -- * Request Lenses
    reloadTables_reloadOption,
    reloadTables_replicationTaskArn,
    reloadTables_tablesToReload,

    -- * Destructuring the Response
    ReloadTablesResponse (..),
    newReloadTablesResponse,

    -- * Response Lenses
    reloadTablesResponse_replicationTaskArn,
    reloadTablesResponse_httpStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReloadTables' smart constructor.
data ReloadTables = ReloadTables'
  { -- | Options for reload. Specify @data-reload@ to reload the data and
    -- re-validate it if validation is enabled. Specify @validate-only@ to
    -- re-validate the table. This option applies only when validation is
    -- enabled for the task.
    --
    -- Valid values: data-reload, validate-only
    --
    -- Default value is data-reload.
    reloadOption :: Prelude.Maybe ReloadOptionValue,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Text,
    -- | The name and schema of the table to be reloaded.
    tablesToReload :: [TableToReload]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReloadTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reloadOption', 'reloadTables_reloadOption' - Options for reload. Specify @data-reload@ to reload the data and
-- re-validate it if validation is enabled. Specify @validate-only@ to
-- re-validate the table. This option applies only when validation is
-- enabled for the task.
--
-- Valid values: data-reload, validate-only
--
-- Default value is data-reload.
--
-- 'replicationTaskArn', 'reloadTables_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'tablesToReload', 'reloadTables_tablesToReload' - The name and schema of the table to be reloaded.
newReloadTables ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  ReloadTables
newReloadTables pReplicationTaskArn_ =
  ReloadTables'
    { reloadOption = Prelude.Nothing,
      replicationTaskArn = pReplicationTaskArn_,
      tablesToReload = Prelude.mempty
    }

-- | Options for reload. Specify @data-reload@ to reload the data and
-- re-validate it if validation is enabled. Specify @validate-only@ to
-- re-validate the table. This option applies only when validation is
-- enabled for the task.
--
-- Valid values: data-reload, validate-only
--
-- Default value is data-reload.
reloadTables_reloadOption :: Lens.Lens' ReloadTables (Prelude.Maybe ReloadOptionValue)
reloadTables_reloadOption = Lens.lens (\ReloadTables' {reloadOption} -> reloadOption) (\s@ReloadTables' {} a -> s {reloadOption = a} :: ReloadTables)

-- | The Amazon Resource Name (ARN) of the replication task.
reloadTables_replicationTaskArn :: Lens.Lens' ReloadTables Prelude.Text
reloadTables_replicationTaskArn = Lens.lens (\ReloadTables' {replicationTaskArn} -> replicationTaskArn) (\s@ReloadTables' {} a -> s {replicationTaskArn = a} :: ReloadTables)

-- | The name and schema of the table to be reloaded.
reloadTables_tablesToReload :: Lens.Lens' ReloadTables [TableToReload]
reloadTables_tablesToReload = Lens.lens (\ReloadTables' {tablesToReload} -> tablesToReload) (\s@ReloadTables' {} a -> s {tablesToReload = a} :: ReloadTables) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest ReloadTables where
  type Rs ReloadTables = ReloadTablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ReloadTablesResponse'
            Prelude.<$> (x Prelude..?> "ReplicationTaskArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReloadTables

instance Prelude.NFData ReloadTables

instance Prelude.ToHeaders ReloadTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.ReloadTables" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ReloadTables where
  toJSON ReloadTables' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ReloadOption" Prelude..=)
              Prelude.<$> reloadOption,
            Prelude.Just
              ("ReplicationTaskArn" Prelude..= replicationTaskArn),
            Prelude.Just
              ("TablesToReload" Prelude..= tablesToReload)
          ]
      )

instance Prelude.ToPath ReloadTables where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ReloadTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReloadTablesResponse' smart constructor.
data ReloadTablesResponse = ReloadTablesResponse'
  { -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReloadTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskArn', 'reloadTablesResponse_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'httpStatus', 'reloadTablesResponse_httpStatus' - The response's http status code.
newReloadTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReloadTablesResponse
newReloadTablesResponse pHttpStatus_ =
  ReloadTablesResponse'
    { replicationTaskArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the replication task.
reloadTablesResponse_replicationTaskArn :: Lens.Lens' ReloadTablesResponse (Prelude.Maybe Prelude.Text)
reloadTablesResponse_replicationTaskArn = Lens.lens (\ReloadTablesResponse' {replicationTaskArn} -> replicationTaskArn) (\s@ReloadTablesResponse' {} a -> s {replicationTaskArn = a} :: ReloadTablesResponse)

-- | The response's http status code.
reloadTablesResponse_httpStatus :: Lens.Lens' ReloadTablesResponse Prelude.Int
reloadTablesResponse_httpStatus = Lens.lens (\ReloadTablesResponse' {httpStatus} -> httpStatus) (\s@ReloadTablesResponse' {} a -> s {httpStatus = a} :: ReloadTablesResponse)

instance Prelude.NFData ReloadTablesResponse
