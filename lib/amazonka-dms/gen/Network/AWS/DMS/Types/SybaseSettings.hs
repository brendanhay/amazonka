-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SybaseSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SybaseSettings
  ( SybaseSettings (..),

    -- * Smart constructor
    mkSybaseSettings,

    -- * Lenses
    ssServerName,
    ssUsername,
    ssPassword,
    ssDatabaseName,
    ssPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines a SAP ASE endpoint.
--
-- /See:/ 'mkSybaseSettings' smart constructor.
data SybaseSettings = SybaseSettings'
  { serverName ::
      Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    databaseName :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SybaseSettings' with the minimum fields required to make a request.
--
-- * 'databaseName' - Database name for the endpoint.
-- * 'password' - Endpoint connection password.
-- * 'port' - Endpoint TCP port.
-- * 'serverName' - Fully qualified domain name of the endpoint.
-- * 'username' - Endpoint connection user name.
mkSybaseSettings ::
  SybaseSettings
mkSybaseSettings =
  SybaseSettings'
    { serverName = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing,
      databaseName = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssServerName :: Lens.Lens' SybaseSettings (Lude.Maybe Lude.Text)
ssServerName = Lens.lens (serverName :: SybaseSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: SybaseSettings)
{-# DEPRECATED ssServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUsername :: Lens.Lens' SybaseSettings (Lude.Maybe Lude.Text)
ssUsername = Lens.lens (username :: SybaseSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: SybaseSettings)
{-# DEPRECATED ssUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPassword :: Lens.Lens' SybaseSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
ssPassword = Lens.lens (password :: SybaseSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: SybaseSettings)
{-# DEPRECATED ssPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDatabaseName :: Lens.Lens' SybaseSettings (Lude.Maybe Lude.Text)
ssDatabaseName = Lens.lens (databaseName :: SybaseSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: SybaseSettings)
{-# DEPRECATED ssDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPort :: Lens.Lens' SybaseSettings (Lude.Maybe Lude.Int)
ssPort = Lens.lens (port :: SybaseSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: SybaseSettings)
{-# DEPRECATED ssPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON SybaseSettings where
  parseJSON =
    Lude.withObject
      "SybaseSettings"
      ( \x ->
          SybaseSettings'
            Lude.<$> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON SybaseSettings where
  toJSON SybaseSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServerName" Lude..=) Lude.<$> serverName,
            ("Username" Lude..=) Lude.<$> username,
            ("Password" Lude..=) Lude.<$> password,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("Port" Lude..=) Lude.<$> port
          ]
      )
