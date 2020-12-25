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
    sDetails,
    sDocumentName,
    sEndDate,
    sOutputUrl,
    sOwner,
    sSessionId,
    sStartDate,
    sStatus,
    sTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DocumentName as Types
import qualified Network.AWS.SSM.Types.SessionDetails as Types
import qualified Network.AWS.SSM.Types.SessionId as Types
import qualified Network.AWS.SSM.Types.SessionManagerOutputUrl as Types
import qualified Network.AWS.SSM.Types.SessionOwner as Types
import qualified Network.AWS.SSM.Types.SessionStatus as Types
import qualified Network.AWS.SSM.Types.SessionTarget as Types

-- | Information about a Session Manager connection to an instance.
--
-- /See:/ 'mkSession' smart constructor.
data Session = Session'
  { -- | Reserved for future use.
    details :: Core.Maybe Types.SessionDetails,
    -- | The name of the Session Manager SSM document used to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ .
    documentName :: Core.Maybe Types.DocumentName,
    -- | The date and time, in ISO-8601 Extended format, when the session was terminated.
    endDate :: Core.Maybe Core.NominalDiffTime,
    -- | Reserved for future use.
    outputUrl :: Core.Maybe Types.SessionManagerOutputUrl,
    -- | The ID of the AWS user account that started the session.
    owner :: Core.Maybe Types.SessionOwner,
    -- | The ID of the session.
    sessionId :: Core.Maybe Types.SessionId,
    -- | The date and time, in ISO-8601 Extended format, when the session began.
    startDate :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the session. For example, "Connected" or "Terminated".
    status :: Core.Maybe Types.SessionStatus,
    -- | The instance that the Session Manager session connected to.
    target :: Core.Maybe Types.SessionTarget
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Session' value with any optional fields omitted.
mkSession ::
  Session
mkSession =
  Session'
    { details = Core.Nothing,
      documentName = Core.Nothing,
      endDate = Core.Nothing,
      outputUrl = Core.Nothing,
      owner = Core.Nothing,
      sessionId = Core.Nothing,
      startDate = Core.Nothing,
      status = Core.Nothing,
      target = Core.Nothing
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDetails :: Lens.Lens' Session (Core.Maybe Types.SessionDetails)
sDetails = Lens.field @"details"
{-# DEPRECATED sDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The name of the Session Manager SSM document used to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ .
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocumentName :: Lens.Lens' Session (Core.Maybe Types.DocumentName)
sDocumentName = Lens.field @"documentName"
{-# DEPRECATED sDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The date and time, in ISO-8601 Extended format, when the session was terminated.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndDate :: Lens.Lens' Session (Core.Maybe Core.NominalDiffTime)
sEndDate = Lens.field @"endDate"
{-# DEPRECATED sEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'outputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutputUrl :: Lens.Lens' Session (Core.Maybe Types.SessionManagerOutputUrl)
sOutputUrl = Lens.field @"outputUrl"
{-# DEPRECATED sOutputUrl "Use generic-lens or generic-optics with 'outputUrl' instead." #-}

-- | The ID of the AWS user account that started the session.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwner :: Lens.Lens' Session (Core.Maybe Types.SessionOwner)
sOwner = Lens.field @"owner"
{-# DEPRECATED sOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The ID of the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSessionId :: Lens.Lens' Session (Core.Maybe Types.SessionId)
sSessionId = Lens.field @"sessionId"
{-# DEPRECATED sSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | The date and time, in ISO-8601 Extended format, when the session began.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartDate :: Lens.Lens' Session (Core.Maybe Core.NominalDiffTime)
sStartDate = Lens.field @"startDate"
{-# DEPRECATED sStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The status of the session. For example, "Connected" or "Terminated".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Session (Core.Maybe Types.SessionStatus)
sStatus = Lens.field @"status"
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The instance that the Session Manager session connected to.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTarget :: Lens.Lens' Session (Core.Maybe Types.SessionTarget)
sTarget = Lens.field @"target"
{-# DEPRECATED sTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Core.FromJSON Session where
  parseJSON =
    Core.withObject "Session" Core.$
      \x ->
        Session'
          Core.<$> (x Core..:? "Details")
          Core.<*> (x Core..:? "DocumentName")
          Core.<*> (x Core..:? "EndDate")
          Core.<*> (x Core..:? "OutputUrl")
          Core.<*> (x Core..:? "Owner")
          Core.<*> (x Core..:? "SessionId")
          Core.<*> (x Core..:? "StartDate")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Target")
