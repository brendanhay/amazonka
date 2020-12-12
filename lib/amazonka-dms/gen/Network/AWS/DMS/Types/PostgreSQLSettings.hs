{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.PostgreSQLSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.PostgreSQLSettings
  ( PostgreSQLSettings (..),

    -- * Smart constructor
    mkPostgreSQLSettings,

    -- * Lenses
    psqlsExecuteTimeout,
    psqlsMaxFileSize,
    psqlsFailTasksOnLobTruncation,
    psqlsServerName,
    psqlsDdlArtifactsSchema,
    psqlsSlotName,
    psqlsUsername,
    psqlsPassword,
    psqlsDatabaseName,
    psqlsAfterConnectScript,
    psqlsCaptureDdls,
    psqlsPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines a PostgreSQL endpoint.
--
-- /See:/ 'mkPostgreSQLSettings' smart constructor.
data PostgreSQLSettings = PostgreSQLSettings'
  { executeTimeout ::
      Lude.Maybe Lude.Int,
    maxFileSize :: Lude.Maybe Lude.Int,
    failTasksOnLobTruncation :: Lude.Maybe Lude.Bool,
    serverName :: Lude.Maybe Lude.Text,
    ddlArtifactsSchema :: Lude.Maybe Lude.Text,
    slotName :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    databaseName :: Lude.Maybe Lude.Text,
    afterConnectScript :: Lude.Maybe Lude.Text,
    captureDdls :: Lude.Maybe Lude.Bool,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostgreSQLSettings' with the minimum fields required to make a request.
--
-- * 'afterConnectScript' - For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role='replica'@
-- * 'captureDdls' - To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@ , you don't have to create tables or triggers on the source database.
-- * 'databaseName' - Database name for the endpoint.
-- * 'ddlArtifactsSchema' - The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
-- * 'executeTimeout' - Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
-- * 'failTasksOnLobTruncation' - When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ .
--
-- If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
-- * 'maxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL.
--
-- Example: @maxFileSize=512@
-- * 'password' - Endpoint connection password.
-- * 'port' - Endpoint TCP port.
-- * 'serverName' - Fully qualified domain name of the endpoint.
-- * 'slotName' - Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
-- * 'username' - Endpoint connection user name.
mkPostgreSQLSettings ::
  PostgreSQLSettings
mkPostgreSQLSettings =
  PostgreSQLSettings'
    { executeTimeout = Lude.Nothing,
      maxFileSize = Lude.Nothing,
      failTasksOnLobTruncation = Lude.Nothing,
      serverName = Lude.Nothing,
      ddlArtifactsSchema = Lude.Nothing,
      slotName = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing,
      databaseName = Lude.Nothing,
      afterConnectScript = Lude.Nothing,
      captureDdls = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Sets the client statement timeout for the PostgreSQL instance, in seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
--
-- /Note:/ Consider using 'executeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsExecuteTimeout :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Int)
psqlsExecuteTimeout = Lens.lens (executeTimeout :: PostgreSQLSettings -> Lude.Maybe Lude.Int) (\s a -> s {executeTimeout = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsExecuteTimeout "Use generic-lens or generic-optics with 'executeTimeout' instead." #-}

-- | Specifies the maximum size (in KB) of any .csv file used to transfer data to PostgreSQL.
--
-- Example: @maxFileSize=512@
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsMaxFileSize :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Int)
psqlsMaxFileSize = Lens.lens (maxFileSize :: PostgreSQLSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxFileSize = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsMaxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead." #-}

-- | When set to @true@ , this value causes a task to fail if the actual size of a LOB column is greater than the specified @LobMaxSize@ .
--
-- If task is set to Limited LOB mode and this option is set to true, the task fails instead of truncating the LOB data.
--
-- /Note:/ Consider using 'failTasksOnLobTruncation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsFailTasksOnLobTruncation :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Bool)
psqlsFailTasksOnLobTruncation = Lens.lens (failTasksOnLobTruncation :: PostgreSQLSettings -> Lude.Maybe Lude.Bool) (\s a -> s {failTasksOnLobTruncation = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsFailTasksOnLobTruncation "Use generic-lens or generic-optics with 'failTasksOnLobTruncation' instead." #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsServerName :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Text)
psqlsServerName = Lens.lens (serverName :: PostgreSQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
--
-- /Note:/ Consider using 'ddlArtifactsSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsDdlArtifactsSchema :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Text)
psqlsDdlArtifactsSchema = Lens.lens (ddlArtifactsSchema :: PostgreSQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {ddlArtifactsSchema = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsDdlArtifactsSchema "Use generic-lens or generic-optics with 'ddlArtifactsSchema' instead." #-}

-- | Sets the name of a previously created logical replication slot for a CDC load of the PostgreSQL source instance.
--
-- When used with the AWS DMS API @CdcStartPosition@ request parameter, this attribute also enables using native CDC start points.
--
-- /Note:/ Consider using 'slotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsSlotName :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Text)
psqlsSlotName = Lens.lens (slotName :: PostgreSQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {slotName = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsSlotName "Use generic-lens or generic-optics with 'slotName' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsUsername :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Text)
psqlsUsername = Lens.lens (username :: PostgreSQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsPassword :: Lens.Lens' PostgreSQLSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
psqlsPassword = Lens.lens (password :: PostgreSQLSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsDatabaseName :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Text)
psqlsDatabaseName = Lens.lens (databaseName :: PostgreSQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | For use with change data capture (CDC) only, this attribute has AWS DMS bypass foreign keys and user triggers to reduce the time it takes to bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role='replica'@
--
-- /Note:/ Consider using 'afterConnectScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsAfterConnectScript :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Text)
psqlsAfterConnectScript = Lens.lens (afterConnectScript :: PostgreSQLSettings -> Lude.Maybe Lude.Text) (\s a -> s {afterConnectScript = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsAfterConnectScript "Use generic-lens or generic-optics with 'afterConnectScript' instead." #-}

-- | To capture DDL events, AWS DMS creates various artifacts in the PostgreSQL database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@ , you don't have to create tables or triggers on the source database.
--
-- /Note:/ Consider using 'captureDdls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsCaptureDdls :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Bool)
psqlsCaptureDdls = Lens.lens (captureDdls :: PostgreSQLSettings -> Lude.Maybe Lude.Bool) (\s a -> s {captureDdls = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsCaptureDdls "Use generic-lens or generic-optics with 'captureDdls' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psqlsPort :: Lens.Lens' PostgreSQLSettings (Lude.Maybe Lude.Int)
psqlsPort = Lens.lens (port :: PostgreSQLSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: PostgreSQLSettings)
{-# DEPRECATED psqlsPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON PostgreSQLSettings where
  parseJSON =
    Lude.withObject
      "PostgreSQLSettings"
      ( \x ->
          PostgreSQLSettings'
            Lude.<$> (x Lude..:? "ExecuteTimeout")
            Lude.<*> (x Lude..:? "MaxFileSize")
            Lude.<*> (x Lude..:? "FailTasksOnLobTruncation")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "DdlArtifactsSchema")
            Lude.<*> (x Lude..:? "SlotName")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "AfterConnectScript")
            Lude.<*> (x Lude..:? "CaptureDdls")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON PostgreSQLSettings where
  toJSON PostgreSQLSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExecuteTimeout" Lude..=) Lude.<$> executeTimeout,
            ("MaxFileSize" Lude..=) Lude.<$> maxFileSize,
            ("FailTasksOnLobTruncation" Lude..=)
              Lude.<$> failTasksOnLobTruncation,
            ("ServerName" Lude..=) Lude.<$> serverName,
            ("DdlArtifactsSchema" Lude..=) Lude.<$> ddlArtifactsSchema,
            ("SlotName" Lude..=) Lude.<$> slotName,
            ("Username" Lude..=) Lude.<$> username,
            ("Password" Lude..=) Lude.<$> password,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("AfterConnectScript" Lude..=) Lude.<$> afterConnectScript,
            ("CaptureDdls" Lude..=) Lude.<$> captureDdls,
            ("Port" Lude..=) Lude.<$> port
          ]
      )
