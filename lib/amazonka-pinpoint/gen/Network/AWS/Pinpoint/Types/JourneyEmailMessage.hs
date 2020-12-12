{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyEmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyEmailMessage
  ( JourneyEmailMessage (..),

    -- * Smart constructor
    mkJourneyEmailMessage,

    -- * Lenses
    jemFromAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the "From" address for an email message that's sent to participants in a journey.
--
-- /See:/ 'mkJourneyEmailMessage' smart constructor.
newtype JourneyEmailMessage = JourneyEmailMessage'
  { fromAddress ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyEmailMessage' with the minimum fields required to make a request.
--
-- * 'fromAddress' - The verified email address to send the email message from. The default address is the FromAddress specified for the email channel for the application.
mkJourneyEmailMessage ::
  JourneyEmailMessage
mkJourneyEmailMessage =
  JourneyEmailMessage' {fromAddress = Lude.Nothing}

-- | The verified email address to send the email message from. The default address is the FromAddress specified for the email channel for the application.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemFromAddress :: Lens.Lens' JourneyEmailMessage (Lude.Maybe Lude.Text)
jemFromAddress = Lens.lens (fromAddress :: JourneyEmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {fromAddress = a} :: JourneyEmailMessage)
{-# DEPRECATED jemFromAddress "Use generic-lens or generic-optics with 'fromAddress' instead." #-}

instance Lude.FromJSON JourneyEmailMessage where
  parseJSON =
    Lude.withObject
      "JourneyEmailMessage"
      (\x -> JourneyEmailMessage' Lude.<$> (x Lude..:? "FromAddress"))

instance Lude.ToJSON JourneyEmailMessage where
  toJSON JourneyEmailMessage' {..} =
    Lude.object
      (Lude.catMaybes [("FromAddress" Lude..=) Lude.<$> fromAddress])
