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

import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Result message wrapping a single description of an application version.
--
-- /See:/ 'mkApplicationVersionDescriptionMessage' smart constructor.
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
  { applicationVersion ::
      Lude.Maybe
        ApplicationVersionDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationVersionDescriptionMessage' with the minimum fields required to make a request.
--
-- * 'applicationVersion' - The 'ApplicationVersionDescription' of the application version.
mkApplicationVersionDescriptionMessage ::
  ApplicationVersionDescriptionMessage
mkApplicationVersionDescriptionMessage =
  ApplicationVersionDescriptionMessage'
    { applicationVersion =
        Lude.Nothing
    }

-- | The 'ApplicationVersionDescription' of the application version.
--
-- /Note:/ Consider using 'applicationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdmApplicationVersion :: Lens.Lens' ApplicationVersionDescriptionMessage (Lude.Maybe ApplicationVersionDescription)
avdmApplicationVersion = Lens.lens (applicationVersion :: ApplicationVersionDescriptionMessage -> Lude.Maybe ApplicationVersionDescription) (\s a -> s {applicationVersion = a} :: ApplicationVersionDescriptionMessage)
{-# DEPRECATED avdmApplicationVersion "Use generic-lens or generic-optics with 'applicationVersion' instead." #-}

instance Lude.FromXML ApplicationVersionDescriptionMessage where
  parseXML x =
    ApplicationVersionDescriptionMessage'
      Lude.<$> (x Lude..@? "ApplicationVersion")
