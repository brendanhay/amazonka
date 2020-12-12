{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.ServerEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.ServerEvent
  ( ServerEvent (..),

    -- * Smart constructor
    mkServerEvent,

    -- * Lenses
    seLogURL,
    seServerName,
    seCreatedAt,
    seMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An event that is related to the server, such as the start of maintenance or backup.
--
-- /See:/ 'mkServerEvent' smart constructor.
data ServerEvent = ServerEvent'
  { logURL :: Lude.Maybe Lude.Text,
    serverName :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerEvent' with the minimum fields required to make a request.
--
-- * 'createdAt' - The time when the event occurred.
-- * 'logURL' - The Amazon S3 URL of the event's log file.
-- * 'message' - A human-readable informational or status message.
-- * 'serverName' - The name of the server on or for which the event occurred.
mkServerEvent ::
  ServerEvent
mkServerEvent =
  ServerEvent'
    { logURL = Lude.Nothing,
      serverName = Lude.Nothing,
      createdAt = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The Amazon S3 URL of the event's log file.
--
-- /Note:/ Consider using 'logURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seLogURL :: Lens.Lens' ServerEvent (Lude.Maybe Lude.Text)
seLogURL = Lens.lens (logURL :: ServerEvent -> Lude.Maybe Lude.Text) (\s a -> s {logURL = a} :: ServerEvent)
{-# DEPRECATED seLogURL "Use generic-lens or generic-optics with 'logURL' instead." #-}

-- | The name of the server on or for which the event occurred.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seServerName :: Lens.Lens' ServerEvent (Lude.Maybe Lude.Text)
seServerName = Lens.lens (serverName :: ServerEvent -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: ServerEvent)
{-# DEPRECATED seServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The time when the event occurred.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCreatedAt :: Lens.Lens' ServerEvent (Lude.Maybe Lude.Timestamp)
seCreatedAt = Lens.lens (createdAt :: ServerEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ServerEvent)
{-# DEPRECATED seCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A human-readable informational or status message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ServerEvent (Lude.Maybe Lude.Text)
seMessage = Lens.lens (message :: ServerEvent -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ServerEvent)
{-# DEPRECATED seMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ServerEvent where
  parseJSON =
    Lude.withObject
      "ServerEvent"
      ( \x ->
          ServerEvent'
            Lude.<$> (x Lude..:? "LogUrl")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "Message")
      )
