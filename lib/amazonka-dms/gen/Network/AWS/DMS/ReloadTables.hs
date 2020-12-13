{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ReloadTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reloads the target database table with the source data.
module Network.AWS.DMS.ReloadTables
  ( -- * Creating a request
    ReloadTables (..),
    mkReloadTables,

    -- ** Request lenses
    rtsTablesToReload,
    rtsReloadOption,
    rtsReplicationTaskARN,

    -- * Destructuring the response
    ReloadTablesResponse (..),
    mkReloadTablesResponse,

    -- ** Response lenses
    rtrsReplicationTaskARN,
    rtrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReloadTables' smart constructor.
data ReloadTables = ReloadTables'
  { -- | The name and schema of the table to be reloaded.
    tablesToReload :: [TableToReload],
    -- | Options for reload. Specify @data-reload@ to reload the data and re-validate it if validation is enabled. Specify @validate-only@ to re-validate the table. This option applies only when validation is enabled for the task.
    --
    -- Valid values: data-reload, validate-only
    -- Default value is data-reload.
    reloadOption :: Lude.Maybe ReloadOptionValue,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReloadTables' with the minimum fields required to make a request.
--
-- * 'tablesToReload' - The name and schema of the table to be reloaded.
-- * 'reloadOption' - Options for reload. Specify @data-reload@ to reload the data and re-validate it if validation is enabled. Specify @validate-only@ to re-validate the table. This option applies only when validation is enabled for the task.
--
-- Valid values: data-reload, validate-only
-- Default value is data-reload.
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
mkReloadTables ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  ReloadTables
mkReloadTables pReplicationTaskARN_ =
  ReloadTables'
    { tablesToReload = Lude.mempty,
      reloadOption = Lude.Nothing,
      replicationTaskARN = pReplicationTaskARN_
    }

-- | The name and schema of the table to be reloaded.
--
-- /Note:/ Consider using 'tablesToReload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTablesToReload :: Lens.Lens' ReloadTables [TableToReload]
rtsTablesToReload = Lens.lens (tablesToReload :: ReloadTables -> [TableToReload]) (\s a -> s {tablesToReload = a} :: ReloadTables)
{-# DEPRECATED rtsTablesToReload "Use generic-lens or generic-optics with 'tablesToReload' instead." #-}

-- | Options for reload. Specify @data-reload@ to reload the data and re-validate it if validation is enabled. Specify @validate-only@ to re-validate the table. This option applies only when validation is enabled for the task.
--
-- Valid values: data-reload, validate-only
-- Default value is data-reload.
--
-- /Note:/ Consider using 'reloadOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsReloadOption :: Lens.Lens' ReloadTables (Lude.Maybe ReloadOptionValue)
rtsReloadOption = Lens.lens (reloadOption :: ReloadTables -> Lude.Maybe ReloadOptionValue) (\s a -> s {reloadOption = a} :: ReloadTables)
{-# DEPRECATED rtsReloadOption "Use generic-lens or generic-optics with 'reloadOption' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsReplicationTaskARN :: Lens.Lens' ReloadTables Lude.Text
rtsReplicationTaskARN = Lens.lens (replicationTaskARN :: ReloadTables -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: ReloadTables)
{-# DEPRECATED rtsReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

instance Lude.AWSRequest ReloadTables where
  type Rs ReloadTables = ReloadTablesResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReloadTablesResponse'
            Lude.<$> (x Lude..?> "ReplicationTaskArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReloadTables where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.ReloadTables" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ReloadTables where
  toJSON ReloadTables' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TablesToReload" Lude..= tablesToReload),
            ("ReloadOption" Lude..=) Lude.<$> reloadOption,
            Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN)
          ]
      )

instance Lude.ToPath ReloadTables where
  toPath = Lude.const "/"

instance Lude.ToQuery ReloadTables where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkReloadTablesResponse' smart constructor.
data ReloadTablesResponse = ReloadTablesResponse'
  { -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReloadTablesResponse' with the minimum fields required to make a request.
--
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
-- * 'responseStatus' - The response status code.
mkReloadTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReloadTablesResponse
mkReloadTablesResponse pResponseStatus_ =
  ReloadTablesResponse'
    { replicationTaskARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsReplicationTaskARN :: Lens.Lens' ReloadTablesResponse (Lude.Maybe Lude.Text)
rtrsReplicationTaskARN = Lens.lens (replicationTaskARN :: ReloadTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: ReloadTablesResponse)
{-# DEPRECATED rtrsReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResponseStatus :: Lens.Lens' ReloadTablesResponse Lude.Int
rtrsResponseStatus = Lens.lens (responseStatus :: ReloadTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReloadTablesResponse)
{-# DEPRECATED rtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
