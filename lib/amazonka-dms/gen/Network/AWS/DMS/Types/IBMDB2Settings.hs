{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.IBMDB2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.IBMDB2Settings
  ( IBMDB2Settings (..),

    -- * Smart constructor
    mkIBMDB2Settings,

    -- * Lenses
    ibmdsServerName,
    ibmdsCurrentLsn,
    ibmdsSetDataCaptureChanges,
    ibmdsUsername,
    ibmdsPassword,
    ibmdsDatabaseName,
    ibmdsMaxKBytesPerRead,
    ibmdsPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines an IBM Db2 LUW endpoint.
--
-- /See:/ 'mkIBMDB2Settings' smart constructor.
data IBMDB2Settings = IBMDB2Settings'
  { serverName ::
      Lude.Maybe Lude.Text,
    currentLsn :: Lude.Maybe Lude.Text,
    setDataCaptureChanges :: Lude.Maybe Lude.Bool,
    username :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    databaseName :: Lude.Maybe Lude.Text,
    maxKBytesPerRead :: Lude.Maybe Lude.Int,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IBMDB2Settings' with the minimum fields required to make a request.
--
-- * 'currentLsn' - For ongoing replication (CDC), use CurrentLSN to specify a log sequence number (LSN) where you want the replication to start.
-- * 'databaseName' - Database name for the endpoint.
-- * 'maxKBytesPerRead' - Maximum number of bytes per read, as a NUMBER value. The default is 64 KB.
-- * 'password' - Endpoint connection password.
-- * 'port' - Endpoint TCP port.
-- * 'serverName' - Fully qualified domain name of the endpoint.
-- * 'setDataCaptureChanges' - Enables ongoing replication (CDC) as a BOOLEAN value. The default is true.
-- * 'username' - Endpoint connection user name.
mkIBMDB2Settings ::
  IBMDB2Settings
mkIBMDB2Settings =
  IBMDB2Settings'
    { serverName = Lude.Nothing,
      currentLsn = Lude.Nothing,
      setDataCaptureChanges = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing,
      databaseName = Lude.Nothing,
      maxKBytesPerRead = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsServerName :: Lens.Lens' IBMDB2Settings (Lude.Maybe Lude.Text)
ibmdsServerName = Lens.lens (serverName :: IBMDB2Settings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence number (LSN) where you want the replication to start.
--
-- /Note:/ Consider using 'currentLsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsCurrentLsn :: Lens.Lens' IBMDB2Settings (Lude.Maybe Lude.Text)
ibmdsCurrentLsn = Lens.lens (currentLsn :: IBMDB2Settings -> Lude.Maybe Lude.Text) (\s a -> s {currentLsn = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsCurrentLsn "Use generic-lens or generic-optics with 'currentLsn' instead." #-}

-- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is true.
--
-- /Note:/ Consider using 'setDataCaptureChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsSetDataCaptureChanges :: Lens.Lens' IBMDB2Settings (Lude.Maybe Lude.Bool)
ibmdsSetDataCaptureChanges = Lens.lens (setDataCaptureChanges :: IBMDB2Settings -> Lude.Maybe Lude.Bool) (\s a -> s {setDataCaptureChanges = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsSetDataCaptureChanges "Use generic-lens or generic-optics with 'setDataCaptureChanges' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsUsername :: Lens.Lens' IBMDB2Settings (Lude.Maybe Lude.Text)
ibmdsUsername = Lens.lens (username :: IBMDB2Settings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsPassword :: Lens.Lens' IBMDB2Settings (Lude.Maybe (Lude.Sensitive Lude.Text))
ibmdsPassword = Lens.lens (password :: IBMDB2Settings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsDatabaseName :: Lens.Lens' IBMDB2Settings (Lude.Maybe Lude.Text)
ibmdsDatabaseName = Lens.lens (databaseName :: IBMDB2Settings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Maximum number of bytes per read, as a NUMBER value. The default is 64 KB.
--
-- /Note:/ Consider using 'maxKBytesPerRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsMaxKBytesPerRead :: Lens.Lens' IBMDB2Settings (Lude.Maybe Lude.Int)
ibmdsMaxKBytesPerRead = Lens.lens (maxKBytesPerRead :: IBMDB2Settings -> Lude.Maybe Lude.Int) (\s a -> s {maxKBytesPerRead = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsMaxKBytesPerRead "Use generic-lens or generic-optics with 'maxKBytesPerRead' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsPort :: Lens.Lens' IBMDB2Settings (Lude.Maybe Lude.Int)
ibmdsPort = Lens.lens (port :: IBMDB2Settings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: IBMDB2Settings)
{-# DEPRECATED ibmdsPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON IBMDB2Settings where
  parseJSON =
    Lude.withObject
      "IBMDB2Settings"
      ( \x ->
          IBMDB2Settings'
            Lude.<$> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "CurrentLsn")
            Lude.<*> (x Lude..:? "SetDataCaptureChanges")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "MaxKBytesPerRead")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON IBMDB2Settings where
  toJSON IBMDB2Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServerName" Lude..=) Lude.<$> serverName,
            ("CurrentLsn" Lude..=) Lude.<$> currentLsn,
            ("SetDataCaptureChanges" Lude..=) Lude.<$> setDataCaptureChanges,
            ("Username" Lude..=) Lude.<$> username,
            ("Password" Lude..=) Lude.<$> password,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("MaxKBytesPerRead" Lude..=) Lude.<$> maxKBytesPerRead,
            ("Port" Lude..=) Lude.<$> port
          ]
      )
