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
-- Module      : Network.AWS.SESV2.Types.Contact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESV2.Types.Contact where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESV2.Types.TopicPreference

-- | A contact is the end-user who is receiving the email.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | A boolean value status noting if the contact is unsubscribed from all
    -- contact list topics.
    unsubscribeAll :: Prelude.Maybe Prelude.Bool,
    -- | The default topic preferences applied to the contact.
    topicDefaultPreferences :: Prelude.Maybe [TopicPreference],
    -- | The contact\'s email address.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | A timestamp noting the last time the contact\'s information was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The contact\'s preference for being opted-in to or opted-out of a topic.
    topicPreferences :: Prelude.Maybe [TopicPreference]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Contact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsubscribeAll', 'contact_unsubscribeAll' - A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
--
-- 'topicDefaultPreferences', 'contact_topicDefaultPreferences' - The default topic preferences applied to the contact.
--
-- 'emailAddress', 'contact_emailAddress' - The contact\'s email address.
--
-- 'lastUpdatedTimestamp', 'contact_lastUpdatedTimestamp' - A timestamp noting the last time the contact\'s information was updated.
--
-- 'topicPreferences', 'contact_topicPreferences' - The contact\'s preference for being opted-in to or opted-out of a topic.
newContact ::
  Contact
newContact =
  Contact'
    { unsubscribeAll = Prelude.Nothing,
      topicDefaultPreferences = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      topicPreferences = Prelude.Nothing
    }

-- | A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
contact_unsubscribeAll :: Lens.Lens' Contact (Prelude.Maybe Prelude.Bool)
contact_unsubscribeAll = Lens.lens (\Contact' {unsubscribeAll} -> unsubscribeAll) (\s@Contact' {} a -> s {unsubscribeAll = a} :: Contact)

-- | The default topic preferences applied to the contact.
contact_topicDefaultPreferences :: Lens.Lens' Contact (Prelude.Maybe [TopicPreference])
contact_topicDefaultPreferences = Lens.lens (\Contact' {topicDefaultPreferences} -> topicDefaultPreferences) (\s@Contact' {} a -> s {topicDefaultPreferences = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

-- | The contact\'s email address.
contact_emailAddress :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_emailAddress = Lens.lens (\Contact' {emailAddress} -> emailAddress) (\s@Contact' {} a -> s {emailAddress = a} :: Contact)

-- | A timestamp noting the last time the contact\'s information was updated.
contact_lastUpdatedTimestamp :: Lens.Lens' Contact (Prelude.Maybe Prelude.UTCTime)
contact_lastUpdatedTimestamp = Lens.lens (\Contact' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@Contact' {} a -> s {lastUpdatedTimestamp = a} :: Contact) Prelude.. Lens.mapping Core._Time

-- | The contact\'s preference for being opted-in to or opted-out of a topic.
contact_topicPreferences :: Lens.Lens' Contact (Prelude.Maybe [TopicPreference])
contact_topicPreferences = Lens.lens (\Contact' {topicPreferences} -> topicPreferences) (\s@Contact' {} a -> s {topicPreferences = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Contact where
  parseJSON =
    Core.withObject
      "Contact"
      ( \x ->
          Contact'
            Prelude.<$> (x Core..:? "UnsubscribeAll")
            Prelude.<*> ( x Core..:? "TopicDefaultPreferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EmailAddress")
            Prelude.<*> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> ( x Core..:? "TopicPreferences"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Contact

instance Prelude.NFData Contact
