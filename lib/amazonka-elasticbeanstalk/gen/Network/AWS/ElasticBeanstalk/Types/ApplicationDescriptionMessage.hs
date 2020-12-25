{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
  ( ApplicationDescriptionMessage (..),

    -- * Smart constructor
    mkApplicationDescriptionMessage,

    -- * Lenses
    admApplication,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Result message containing a single description of an application.
--
-- /See:/ 'mkApplicationDescriptionMessage' smart constructor.
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'
  { -- | The 'ApplicationDescription' of the application.
    application :: Core.Maybe Types.ApplicationDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'ApplicationDescriptionMessage' value with any optional fields omitted.
mkApplicationDescriptionMessage ::
  ApplicationDescriptionMessage
mkApplicationDescriptionMessage =
  ApplicationDescriptionMessage' {application = Core.Nothing}

-- | The 'ApplicationDescription' of the application.
--
-- /Note:/ Consider using 'application' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admApplication :: Lens.Lens' ApplicationDescriptionMessage (Core.Maybe Types.ApplicationDescription)
admApplication = Lens.field @"application"
{-# DEPRECATED admApplication "Use generic-lens or generic-optics with 'application' instead." #-}

instance Core.FromXML ApplicationDescriptionMessage where
  parseXML x =
    ApplicationDescriptionMessage'
      Core.<$> (x Core..@? "Application")
