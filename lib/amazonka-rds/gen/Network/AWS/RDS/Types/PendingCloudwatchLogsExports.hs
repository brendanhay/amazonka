{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingCloudwatchLogsExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.PendingCloudwatchLogsExports
  ( PendingCloudwatchLogsExports (..),

    -- * Smart constructor
    mkPendingCloudwatchLogsExports,

    -- * Lenses
    pcleLogTypesToEnable,
    pcleLogTypesToDisable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of the log types whose configuration is still pending. In other words, these log types are in the process of being activated or deactivated.
--
-- /See:/ 'mkPendingCloudwatchLogsExports' smart constructor.
data PendingCloudwatchLogsExports = PendingCloudwatchLogsExports'
  { logTypesToEnable ::
      Lude.Maybe [Lude.Text],
    logTypesToDisable ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingCloudwatchLogsExports' with the minimum fields required to make a request.
--
-- * 'logTypesToDisable' - Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
-- * 'logTypesToEnable' - Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
mkPendingCloudwatchLogsExports ::
  PendingCloudwatchLogsExports
mkPendingCloudwatchLogsExports =
  PendingCloudwatchLogsExports'
    { logTypesToEnable = Lude.Nothing,
      logTypesToDisable = Lude.Nothing
    }

-- | Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
--
-- /Note:/ Consider using 'logTypesToEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcleLogTypesToEnable :: Lens.Lens' PendingCloudwatchLogsExports (Lude.Maybe [Lude.Text])
pcleLogTypesToEnable = Lens.lens (logTypesToEnable :: PendingCloudwatchLogsExports -> Lude.Maybe [Lude.Text]) (\s a -> s {logTypesToEnable = a} :: PendingCloudwatchLogsExports)
{-# DEPRECATED pcleLogTypesToEnable "Use generic-lens or generic-optics with 'logTypesToEnable' instead." #-}

-- | Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
--
-- /Note:/ Consider using 'logTypesToDisable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcleLogTypesToDisable :: Lens.Lens' PendingCloudwatchLogsExports (Lude.Maybe [Lude.Text])
pcleLogTypesToDisable = Lens.lens (logTypesToDisable :: PendingCloudwatchLogsExports -> Lude.Maybe [Lude.Text]) (\s a -> s {logTypesToDisable = a} :: PendingCloudwatchLogsExports)
{-# DEPRECATED pcleLogTypesToDisable "Use generic-lens or generic-optics with 'logTypesToDisable' instead." #-}

instance Lude.FromXML PendingCloudwatchLogsExports where
  parseXML x =
    PendingCloudwatchLogsExports'
      Lude.<$> ( x Lude..@? "LogTypesToEnable" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "LogTypesToDisable" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
