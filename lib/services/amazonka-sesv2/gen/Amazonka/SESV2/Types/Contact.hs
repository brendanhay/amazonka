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
-- Module      : Amazonka.SESV2.Types.Contact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Contact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.TopicPreference

-- | A contact is the end-user who is receiving the email.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | A timestamp noting the last time the contact\'s information was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | A boolean value status noting if the contact is unsubscribed from all
    -- contact list topics.
    unsubscribeAll :: Prelude.Maybe Prelude.Bool,
    -- | The contact\'s preference for being opted-in to or opted-out of a topic.
    topicPreferences :: Prelude.Maybe [TopicPreference],
    -- | The default topic preferences applied to the contact.
    topicDefaultPreferences :: Prelude.Maybe [TopicPreference],
    -- | The contact\'s email address.
    emailAddress :: Prelude.Maybe Prelude.Text
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
-- 'lastUpdatedTimestamp', 'contact_lastUpdatedTimestamp' - A timestamp noting the last time the contact\'s information was updated.
--
-- 'unsubscribeAll', 'contact_unsubscribeAll' - A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
--
-- 'topicPreferences', 'contact_topicPreferences' - The contact\'s preference for being opted-in to or opted-out of a topic.
--
-- 'topicDefaultPreferences', 'contact_topicDefaultPreferences' - The default topic preferences applied to the contact.
--
-- 'emailAddress', 'contact_emailAddress' - The contact\'s email address.
newContact ::
  Contact
newContact =
  Contact'
    { lastUpdatedTimestamp = Prelude.Nothing,
      unsubscribeAll = Prelude.Nothing,
      topicPreferences = Prelude.Nothing,
      topicDefaultPreferences = Prelude.Nothing,
      emailAddress = Prelude.Nothing
    }

-- | A timestamp noting the last time the contact\'s information was updated.
contact_lastUpdatedTimestamp :: Lens.Lens' Contact (Prelude.Maybe Prelude.UTCTime)
contact_lastUpdatedTimestamp = Lens.lens (\Contact' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@Contact' {} a -> s {lastUpdatedTimestamp = a} :: Contact) Prelude.. Lens.mapping Core._Time

-- | A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
contact_unsubscribeAll :: Lens.Lens' Contact (Prelude.Maybe Prelude.Bool)
contact_unsubscribeAll = Lens.lens (\Contact' {unsubscribeAll} -> unsubscribeAll) (\s@Contact' {} a -> s {unsubscribeAll = a} :: Contact)

-- | The contact\'s preference for being opted-in to or opted-out of a topic.
contact_topicPreferences :: Lens.Lens' Contact (Prelude.Maybe [TopicPreference])
contact_topicPreferences = Lens.lens (\Contact' {topicPreferences} -> topicPreferences) (\s@Contact' {} a -> s {topicPreferences = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

-- | The default topic preferences applied to the contact.
contact_topicDefaultPreferences :: Lens.Lens' Contact (Prelude.Maybe [TopicPreference])
contact_topicDefaultPreferences = Lens.lens (\Contact' {topicDefaultPreferences} -> topicDefaultPreferences) (\s@Contact' {} a -> s {topicDefaultPreferences = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

-- | The contact\'s email address.
contact_emailAddress :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_emailAddress = Lens.lens (\Contact' {emailAddress} -> emailAddress) (\s@Contact' {} a -> s {emailAddress = a} :: Contact)

instance Core.FromJSON Contact where
  parseJSON =
    Core.withObject
      "Contact"
      ( \x ->
          Contact'
            Prelude.<$> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "UnsubscribeAll")
            Prelude.<*> ( x Core..:? "TopicPreferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "TopicDefaultPreferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EmailAddress")
      )

instance Prelude.Hashable Contact where
  hashWithSalt _salt Contact' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` unsubscribeAll
      `Prelude.hashWithSalt` topicPreferences
      `Prelude.hashWithSalt` topicDefaultPreferences
      `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData Contact where
  rnf Contact' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf unsubscribeAll
      `Prelude.seq` Prelude.rnf topicPreferences
      `Prelude.seq` Prelude.rnf topicDefaultPreferences
      `Prelude.seq` Prelude.rnf emailAddress
