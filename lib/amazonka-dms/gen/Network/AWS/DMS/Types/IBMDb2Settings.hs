{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.IBMDb2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.IBMDb2Settings
  ( IBMDb2Settings (..)
  -- * Smart constructor
  , mkIBMDb2Settings
  -- * Lenses
  , ibmdsCurrentLsn
  , ibmdsDatabaseName
  , ibmdsMaxKBytesPerRead
  , ibmdsPassword
  , ibmdsPort
  , ibmdsServerName
  , ibmdsSetDataCaptureChanges
  , ibmdsUsername
  ) where

import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines an IBM Db2 LUW endpoint.
--
-- /See:/ 'mkIBMDb2Settings' smart constructor.
data IBMDb2Settings = IBMDb2Settings'
  { currentLsn :: Core.Maybe Core.Text
    -- ^ For ongoing replication (CDC), use CurrentLSN to specify a log sequence number (LSN) where you want the replication to start.
  , databaseName :: Core.Maybe Core.Text
    -- ^ Database name for the endpoint.
  , maxKBytesPerRead :: Core.Maybe Core.Int
    -- ^ Maximum number of bytes per read, as a NUMBER value. The default is 64 KB.
  , password :: Core.Maybe Types.Password
    -- ^ Endpoint connection password.
  , port :: Core.Maybe Core.Int
    -- ^ Endpoint TCP port.
  , serverName :: Core.Maybe Core.Text
    -- ^ Fully qualified domain name of the endpoint.
  , setDataCaptureChanges :: Core.Maybe Core.Bool
    -- ^ Enables ongoing replication (CDC) as a BOOLEAN value. The default is true.
  , username :: Core.Maybe Core.Text
    -- ^ Endpoint connection user name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IBMDb2Settings' value with any optional fields omitted.
mkIBMDb2Settings
    :: IBMDb2Settings
mkIBMDb2Settings
  = IBMDb2Settings'{currentLsn = Core.Nothing,
                    databaseName = Core.Nothing, maxKBytesPerRead = Core.Nothing,
                    password = Core.Nothing, port = Core.Nothing,
                    serverName = Core.Nothing, setDataCaptureChanges = Core.Nothing,
                    username = Core.Nothing}

-- | For ongoing replication (CDC), use CurrentLSN to specify a log sequence number (LSN) where you want the replication to start.
--
-- /Note:/ Consider using 'currentLsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsCurrentLsn :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
ibmdsCurrentLsn = Lens.field @"currentLsn"
{-# INLINEABLE ibmdsCurrentLsn #-}
{-# DEPRECATED currentLsn "Use generic-lens or generic-optics with 'currentLsn' instead"  #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsDatabaseName :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
ibmdsDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE ibmdsDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Maximum number of bytes per read, as a NUMBER value. The default is 64 KB.
--
-- /Note:/ Consider using 'maxKBytesPerRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsMaxKBytesPerRead :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Int)
ibmdsMaxKBytesPerRead = Lens.field @"maxKBytesPerRead"
{-# INLINEABLE ibmdsMaxKBytesPerRead #-}
{-# DEPRECATED maxKBytesPerRead "Use generic-lens or generic-optics with 'maxKBytesPerRead' instead"  #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsPassword :: Lens.Lens' IBMDb2Settings (Core.Maybe Types.Password)
ibmdsPassword = Lens.field @"password"
{-# INLINEABLE ibmdsPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsPort :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Int)
ibmdsPort = Lens.field @"port"
{-# INLINEABLE ibmdsPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsServerName :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
ibmdsServerName = Lens.field @"serverName"
{-# INLINEABLE ibmdsServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | Enables ongoing replication (CDC) as a BOOLEAN value. The default is true.
--
-- /Note:/ Consider using 'setDataCaptureChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsSetDataCaptureChanges :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Bool)
ibmdsSetDataCaptureChanges = Lens.field @"setDataCaptureChanges"
{-# INLINEABLE ibmdsSetDataCaptureChanges #-}
{-# DEPRECATED setDataCaptureChanges "Use generic-lens or generic-optics with 'setDataCaptureChanges' instead"  #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibmdsUsername :: Lens.Lens' IBMDb2Settings (Core.Maybe Core.Text)
ibmdsUsername = Lens.field @"username"
{-# INLINEABLE ibmdsUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON IBMDb2Settings where
        toJSON IBMDb2Settings{..}
          = Core.object
              (Core.catMaybes
                 [("CurrentLsn" Core..=) Core.<$> currentLsn,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("MaxKBytesPerRead" Core..=) Core.<$> maxKBytesPerRead,
                  ("Password" Core..=) Core.<$> password,
                  ("Port" Core..=) Core.<$> port,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("SetDataCaptureChanges" Core..=) Core.<$> setDataCaptureChanges,
                  ("Username" Core..=) Core.<$> username])

instance Core.FromJSON IBMDb2Settings where
        parseJSON
          = Core.withObject "IBMDb2Settings" Core.$
              \ x ->
                IBMDb2Settings' Core.<$>
                  (x Core..:? "CurrentLsn") Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "MaxKBytesPerRead"
                    Core.<*> x Core..:? "Password"
                    Core.<*> x Core..:? "Port"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "SetDataCaptureChanges"
                    Core.<*> x Core..:? "Username"
