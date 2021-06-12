{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType where

import Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the event feedback type.
--
-- /See:/ 'newEventFeedbackType' smart constructor.
data EventFeedbackType = EventFeedbackType'
  { -- | The event feedback date.
    feedbackDate :: Core.Maybe Core.POSIX,
    -- | The event feedback value.
    feedbackValue :: FeedbackValueType,
    -- | The provider.
    provider :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventFeedbackType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feedbackDate', 'eventFeedbackType_feedbackDate' - The event feedback date.
--
-- 'feedbackValue', 'eventFeedbackType_feedbackValue' - The event feedback value.
--
-- 'provider', 'eventFeedbackType_provider' - The provider.
newEventFeedbackType ::
  -- | 'feedbackValue'
  FeedbackValueType ->
  -- | 'provider'
  Core.Text ->
  EventFeedbackType
newEventFeedbackType pFeedbackValue_ pProvider_ =
  EventFeedbackType'
    { feedbackDate = Core.Nothing,
      feedbackValue = pFeedbackValue_,
      provider = pProvider_
    }

-- | The event feedback date.
eventFeedbackType_feedbackDate :: Lens.Lens' EventFeedbackType (Core.Maybe Core.UTCTime)
eventFeedbackType_feedbackDate = Lens.lens (\EventFeedbackType' {feedbackDate} -> feedbackDate) (\s@EventFeedbackType' {} a -> s {feedbackDate = a} :: EventFeedbackType) Core.. Lens.mapping Core._Time

-- | The event feedback value.
eventFeedbackType_feedbackValue :: Lens.Lens' EventFeedbackType FeedbackValueType
eventFeedbackType_feedbackValue = Lens.lens (\EventFeedbackType' {feedbackValue} -> feedbackValue) (\s@EventFeedbackType' {} a -> s {feedbackValue = a} :: EventFeedbackType)

-- | The provider.
eventFeedbackType_provider :: Lens.Lens' EventFeedbackType Core.Text
eventFeedbackType_provider = Lens.lens (\EventFeedbackType' {provider} -> provider) (\s@EventFeedbackType' {} a -> s {provider = a} :: EventFeedbackType)

instance Core.FromJSON EventFeedbackType where
  parseJSON =
    Core.withObject
      "EventFeedbackType"
      ( \x ->
          EventFeedbackType'
            Core.<$> (x Core..:? "FeedbackDate")
            Core.<*> (x Core..: "FeedbackValue")
            Core.<*> (x Core..: "Provider")
      )

instance Core.Hashable EventFeedbackType

instance Core.NFData EventFeedbackType
