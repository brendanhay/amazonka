{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
  ( ApplicationVersionDescriptionMessage (..),

    -- * Smart constructor
    mkApplicationVersionDescriptionMessage,

    -- * Lenses
    avdmApplicationVersion,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Result message wrapping a single description of an application version.
--
-- /See:/ 'mkApplicationVersionDescriptionMessage' smart constructor.
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
  { -- | The 'ApplicationVersionDescription' of the application version.
    applicationVersion :: Core.Maybe Types.ApplicationVersionDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'ApplicationVersionDescriptionMessage' value with any optional fields omitted.
mkApplicationVersionDescriptionMessage ::
  ApplicationVersionDescriptionMessage
mkApplicationVersionDescriptionMessage =
  ApplicationVersionDescriptionMessage'
    { applicationVersion =
        Core.Nothing
    }

-- | The 'ApplicationVersionDescription' of the application version.
--
-- /Note:/ Consider using 'applicationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdmApplicationVersion :: Lens.Lens' ApplicationVersionDescriptionMessage (Core.Maybe Types.ApplicationVersionDescription)
avdmApplicationVersion = Lens.field @"applicationVersion"
{-# DEPRECATED avdmApplicationVersion "Use generic-lens or generic-optics with 'applicationVersion' instead." #-}

instance Core.FromXML ApplicationVersionDescriptionMessage where
  parseXML x =
    ApplicationVersionDescriptionMessage'
      Core.<$> (x Core..@? "ApplicationVersion")
