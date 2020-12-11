-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
  ( EventFeedbackType (..),

    -- * Smart constructor
    mkEventFeedbackType,

    -- * Lenses
    eftFeedbackDate,
    eftFeedbackValue,
    eftProvider,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the event feedback type.
--
-- /See:/ 'mkEventFeedbackType' smart constructor.
data EventFeedbackType = EventFeedbackType'
  { feedbackDate ::
      Lude.Maybe Lude.Timestamp,
    feedbackValue :: FeedbackValueType,
    provider :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventFeedbackType' with the minimum fields required to make a request.
--
-- * 'feedbackDate' - The event feedback date.
-- * 'feedbackValue' - The event feedback value.
-- * 'provider' - The provider.
mkEventFeedbackType ::
  -- | 'feedbackValue'
  FeedbackValueType ->
  -- | 'provider'
  Lude.Text ->
  EventFeedbackType
mkEventFeedbackType pFeedbackValue_ pProvider_ =
  EventFeedbackType'
    { feedbackDate = Lude.Nothing,
      feedbackValue = pFeedbackValue_,
      provider = pProvider_
    }

-- | The event feedback date.
--
-- /Note:/ Consider using 'feedbackDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eftFeedbackDate :: Lens.Lens' EventFeedbackType (Lude.Maybe Lude.Timestamp)
eftFeedbackDate = Lens.lens (feedbackDate :: EventFeedbackType -> Lude.Maybe Lude.Timestamp) (\s a -> s {feedbackDate = a} :: EventFeedbackType)
{-# DEPRECATED eftFeedbackDate "Use generic-lens or generic-optics with 'feedbackDate' instead." #-}

-- | The event feedback value.
--
-- /Note:/ Consider using 'feedbackValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eftFeedbackValue :: Lens.Lens' EventFeedbackType FeedbackValueType
eftFeedbackValue = Lens.lens (feedbackValue :: EventFeedbackType -> FeedbackValueType) (\s a -> s {feedbackValue = a} :: EventFeedbackType)
{-# DEPRECATED eftFeedbackValue "Use generic-lens or generic-optics with 'feedbackValue' instead." #-}

-- | The provider.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eftProvider :: Lens.Lens' EventFeedbackType Lude.Text
eftProvider = Lens.lens (provider :: EventFeedbackType -> Lude.Text) (\s a -> s {provider = a} :: EventFeedbackType)
{-# DEPRECATED eftProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

instance Lude.FromJSON EventFeedbackType where
  parseJSON =
    Lude.withObject
      "EventFeedbackType"
      ( \x ->
          EventFeedbackType'
            Lude.<$> (x Lude..:? "FeedbackDate")
            Lude.<*> (x Lude..: "FeedbackValue")
            Lude.<*> (x Lude..: "Provider")
      )
