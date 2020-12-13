{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Session
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Session
  ( Session (..),

    -- * Smart constructor
    mkSession,

    -- * Lenses
    sStatus,
    sOutputURL,
    sDocumentName,
    sEndDate,
    sOwner,
    sStartDate,
    sDetails,
    sSessionId,
    sTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.SessionManagerOutputURL
import Network.AWS.SSM.Types.SessionStatus

-- | Information about a Session Manager connection to an instance.
--
-- /See:/ 'mkSession' smart constructor.
data Session = Session'
  { -- | The status of the session. For example, "Connected" or "Terminated".
    status :: Lude.Maybe SessionStatus,
    -- | Reserved for future use.
    outputURL :: Lude.Maybe SessionManagerOutputURL,
    -- | The name of the Session Manager SSM document used to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ .
    documentName :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session was terminated.
    endDate :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the AWS user account that started the session.
    owner :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO-8601 Extended format, when the session began.
    startDate :: Lude.Maybe Lude.Timestamp,
    -- | Reserved for future use.
    details :: Lude.Maybe Lude.Text,
    -- | The ID of the session.
    sessionId :: Lude.Maybe Lude.Text,
    -- | The instance that the Session Manager session connected to.
    target :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- * 'status' - The status of the session. For example, "Connected" or "Terminated".
-- * 'outputURL' - Reserved for future use.
-- * 'documentName' - The name of the Session Manager SSM document used to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ .
-- * 'endDate' - The date and time, in ISO-8601 Extended format, when the session was terminated.
-- * 'owner' - The ID of the AWS user account that started the session.
-- * 'startDate' - The date and time, in ISO-8601 Extended format, when the session began.
-- * 'details' - Reserved for future use.
-- * 'sessionId' - The ID of the session.
-- * 'target' - The instance that the Session Manager session connected to.
mkSession ::
  Session
mkSession =
  Session'
    { status = Lude.Nothing,
      outputURL = Lude.Nothing,
      documentName = Lude.Nothing,
      endDate = Lude.Nothing,
      owner = Lude.Nothing,
      startDate = Lude.Nothing,
      details = Lude.Nothing,
      sessionId = Lude.Nothing,
      target = Lude.Nothing
    }

-- | The status of the session. For example, "Connected" or "Terminated".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Session (Lude.Maybe SessionStatus)
sStatus = Lens.lens (status :: Session -> Lude.Maybe SessionStatus) (\s a -> s {status = a} :: Session)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'outputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutputURL :: Lens.Lens' Session (Lude.Maybe SessionManagerOutputURL)
sOutputURL = Lens.lens (outputURL :: Session -> Lude.Maybe SessionManagerOutputURL) (\s a -> s {outputURL = a} :: Session)
{-# DEPRECATED sOutputURL "Use generic-lens or generic-optics with 'outputURL' instead." #-}

-- | The name of the Session Manager SSM document used to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ .
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocumentName :: Lens.Lens' Session (Lude.Maybe Lude.Text)
sDocumentName = Lens.lens (documentName :: Session -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: Session)
{-# DEPRECATED sDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The date and time, in ISO-8601 Extended format, when the session was terminated.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndDate :: Lens.Lens' Session (Lude.Maybe Lude.Timestamp)
sEndDate = Lens.lens (endDate :: Session -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDate = a} :: Session)
{-# DEPRECATED sEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The ID of the AWS user account that started the session.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwner :: Lens.Lens' Session (Lude.Maybe Lude.Text)
sOwner = Lens.lens (owner :: Session -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: Session)
{-# DEPRECATED sOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The date and time, in ISO-8601 Extended format, when the session began.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartDate :: Lens.Lens' Session (Lude.Maybe Lude.Timestamp)
sStartDate = Lens.lens (startDate :: Session -> Lude.Maybe Lude.Timestamp) (\s a -> s {startDate = a} :: Session)
{-# DEPRECATED sStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDetails :: Lens.Lens' Session (Lude.Maybe Lude.Text)
sDetails = Lens.lens (details :: Session -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: Session)
{-# DEPRECATED sDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSessionId :: Lens.Lens' Session (Lude.Maybe Lude.Text)
sSessionId = Lens.lens (sessionId :: Session -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: Session)
{-# DEPRECATED sSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | The instance that the Session Manager session connected to.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTarget :: Lens.Lens' Session (Lude.Maybe Lude.Text)
sTarget = Lens.lens (target :: Session -> Lude.Maybe Lude.Text) (\s a -> s {target = a} :: Session)
{-# DEPRECATED sTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON Session where
  parseJSON =
    Lude.withObject
      "Session"
      ( \x ->
          Session'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "OutputUrl")
            Lude.<*> (x Lude..:? "DocumentName")
            Lude.<*> (x Lude..:? "EndDate")
            Lude.<*> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "StartDate")
            Lude.<*> (x Lude..:? "Details")
            Lude.<*> (x Lude..:? "SessionId")
            Lude.<*> (x Lude..:? "Target")
      )
