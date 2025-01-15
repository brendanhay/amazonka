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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Contact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.TopicPreference

-- | A contact is the end-user who is receiving the email.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | The contact\'s email address.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | A timestamp noting the last time the contact\'s information was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The default topic preferences applied to the contact.
    topicDefaultPreferences :: Prelude.Maybe [TopicPreference],
    -- | The contact\'s preference for being opted-in to or opted-out of a topic.
    topicPreferences :: Prelude.Maybe [TopicPreference],
    -- | A boolean value status noting if the contact is unsubscribed from all
    -- contact list topics.
    unsubscribeAll :: Prelude.Maybe Prelude.Bool
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
-- 'emailAddress', 'contact_emailAddress' - The contact\'s email address.
--
-- 'lastUpdatedTimestamp', 'contact_lastUpdatedTimestamp' - A timestamp noting the last time the contact\'s information was updated.
--
-- 'topicDefaultPreferences', 'contact_topicDefaultPreferences' - The default topic preferences applied to the contact.
--
-- 'topicPreferences', 'contact_topicPreferences' - The contact\'s preference for being opted-in to or opted-out of a topic.
--
-- 'unsubscribeAll', 'contact_unsubscribeAll' - A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
newContact ::
  Contact
newContact =
  Contact'
    { emailAddress = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      topicDefaultPreferences = Prelude.Nothing,
      topicPreferences = Prelude.Nothing,
      unsubscribeAll = Prelude.Nothing
    }

-- | The contact\'s email address.
contact_emailAddress :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_emailAddress = Lens.lens (\Contact' {emailAddress} -> emailAddress) (\s@Contact' {} a -> s {emailAddress = a} :: Contact)

-- | A timestamp noting the last time the contact\'s information was updated.
contact_lastUpdatedTimestamp :: Lens.Lens' Contact (Prelude.Maybe Prelude.UTCTime)
contact_lastUpdatedTimestamp = Lens.lens (\Contact' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@Contact' {} a -> s {lastUpdatedTimestamp = a} :: Contact) Prelude.. Lens.mapping Data._Time

-- | The default topic preferences applied to the contact.
contact_topicDefaultPreferences :: Lens.Lens' Contact (Prelude.Maybe [TopicPreference])
contact_topicDefaultPreferences = Lens.lens (\Contact' {topicDefaultPreferences} -> topicDefaultPreferences) (\s@Contact' {} a -> s {topicDefaultPreferences = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

-- | The contact\'s preference for being opted-in to or opted-out of a topic.
contact_topicPreferences :: Lens.Lens' Contact (Prelude.Maybe [TopicPreference])
contact_topicPreferences = Lens.lens (\Contact' {topicPreferences} -> topicPreferences) (\s@Contact' {} a -> s {topicPreferences = a} :: Contact) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
contact_unsubscribeAll :: Lens.Lens' Contact (Prelude.Maybe Prelude.Bool)
contact_unsubscribeAll = Lens.lens (\Contact' {unsubscribeAll} -> unsubscribeAll) (\s@Contact' {} a -> s {unsubscribeAll = a} :: Contact)

instance Data.FromJSON Contact where
  parseJSON =
    Data.withObject
      "Contact"
      ( \x ->
          Contact'
            Prelude.<$> (x Data..:? "EmailAddress")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> ( x
                            Data..:? "TopicDefaultPreferences"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "TopicPreferences"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UnsubscribeAll")
      )

instance Prelude.Hashable Contact where
  hashWithSalt _salt Contact' {..} =
    _salt
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` topicDefaultPreferences
      `Prelude.hashWithSalt` topicPreferences
      `Prelude.hashWithSalt` unsubscribeAll

instance Prelude.NFData Contact where
  rnf Contact' {..} =
    Prelude.rnf emailAddress `Prelude.seq`
      Prelude.rnf lastUpdatedTimestamp `Prelude.seq`
        Prelude.rnf topicDefaultPreferences `Prelude.seq`
          Prelude.rnf topicPreferences `Prelude.seq`
            Prelude.rnf unsubscribeAll
