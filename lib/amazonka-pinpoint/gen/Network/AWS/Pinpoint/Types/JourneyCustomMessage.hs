{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyCustomMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyCustomMessage
  ( JourneyCustomMessage (..),

    -- * Smart constructor
    mkJourneyCustomMessage,

    -- * Lenses
    jcmData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the message content for a custom channel message that's sent to participants in a journey.
--
-- /See:/ 'mkJourneyCustomMessage' smart constructor.
newtype JourneyCustomMessage = JourneyCustomMessage'
  { -- | The message content that's passed to an AWS Lambda function or to a web hook.
    data' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyCustomMessage' with the minimum fields required to make a request.
--
-- * 'data'' - The message content that's passed to an AWS Lambda function or to a web hook.
mkJourneyCustomMessage ::
  JourneyCustomMessage
mkJourneyCustomMessage =
  JourneyCustomMessage' {data' = Lude.Nothing}

-- | The message content that's passed to an AWS Lambda function or to a web hook.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcmData :: Lens.Lens' JourneyCustomMessage (Lude.Maybe Lude.Text)
jcmData = Lens.lens (data' :: JourneyCustomMessage -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: JourneyCustomMessage)
{-# DEPRECATED jcmData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Lude.FromJSON JourneyCustomMessage where
  parseJSON =
    Lude.withObject
      "JourneyCustomMessage"
      (\x -> JourneyCustomMessage' Lude.<$> (x Lude..:? "Data"))

instance Lude.ToJSON JourneyCustomMessage where
  toJSON JourneyCustomMessage' {..} =
    Lude.object (Lude.catMaybes [("Data" Lude..=) Lude.<$> data'])
