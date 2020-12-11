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

import Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Result message containing a single description of an application.
--
-- /See:/ 'mkApplicationDescriptionMessage' smart constructor.
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'
  { application ::
      Lude.Maybe
        ApplicationDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationDescriptionMessage' with the minimum fields required to make a request.
--
-- * 'application' - The 'ApplicationDescription' of the application.
mkApplicationDescriptionMessage ::
  ApplicationDescriptionMessage
mkApplicationDescriptionMessage =
  ApplicationDescriptionMessage' {application = Lude.Nothing}

-- | The 'ApplicationDescription' of the application.
--
-- /Note:/ Consider using 'application' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admApplication :: Lens.Lens' ApplicationDescriptionMessage (Lude.Maybe ApplicationDescription)
admApplication = Lens.lens (application :: ApplicationDescriptionMessage -> Lude.Maybe ApplicationDescription) (\s a -> s {application = a} :: ApplicationDescriptionMessage)
{-# DEPRECATED admApplication "Use generic-lens or generic-optics with 'application' instead." #-}

instance Lude.FromXML ApplicationDescriptionMessage where
  parseXML x =
    ApplicationDescriptionMessage'
      Lude.<$> (x Lude..@? "Application")
