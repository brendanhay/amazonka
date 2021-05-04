{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the event feedback type.
--
-- /See:/ 'newEventFeedbackType' smart constructor.
data EventFeedbackType = EventFeedbackType'
  { -- | The event feedback date.
    feedbackDate :: Prelude.Maybe Prelude.POSIX,
    -- | The event feedback value.
    feedbackValue :: FeedbackValueType,
    -- | The provider.
    provider :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  EventFeedbackType
newEventFeedbackType pFeedbackValue_ pProvider_ =
  EventFeedbackType'
    { feedbackDate = Prelude.Nothing,
      feedbackValue = pFeedbackValue_,
      provider = pProvider_
    }

-- | The event feedback date.
eventFeedbackType_feedbackDate :: Lens.Lens' EventFeedbackType (Prelude.Maybe Prelude.UTCTime)
eventFeedbackType_feedbackDate = Lens.lens (\EventFeedbackType' {feedbackDate} -> feedbackDate) (\s@EventFeedbackType' {} a -> s {feedbackDate = a} :: EventFeedbackType) Prelude.. Lens.mapping Prelude._Time

-- | The event feedback value.
eventFeedbackType_feedbackValue :: Lens.Lens' EventFeedbackType FeedbackValueType
eventFeedbackType_feedbackValue = Lens.lens (\EventFeedbackType' {feedbackValue} -> feedbackValue) (\s@EventFeedbackType' {} a -> s {feedbackValue = a} :: EventFeedbackType)

-- | The provider.
eventFeedbackType_provider :: Lens.Lens' EventFeedbackType Prelude.Text
eventFeedbackType_provider = Lens.lens (\EventFeedbackType' {provider} -> provider) (\s@EventFeedbackType' {} a -> s {provider = a} :: EventFeedbackType)

instance Prelude.FromJSON EventFeedbackType where
  parseJSON =
    Prelude.withObject
      "EventFeedbackType"
      ( \x ->
          EventFeedbackType'
            Prelude.<$> (x Prelude..:? "FeedbackDate")
            Prelude.<*> (x Prelude..: "FeedbackValue")
            Prelude.<*> (x Prelude..: "Provider")
      )

instance Prelude.Hashable EventFeedbackType

instance Prelude.NFData EventFeedbackType
