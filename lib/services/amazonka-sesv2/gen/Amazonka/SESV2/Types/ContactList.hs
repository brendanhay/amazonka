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
-- Module      : Amazonka.SESV2.Types.ContactList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ContactList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list that contains contacts that have subscribed to a particular topic
-- or topics.
--
-- /See:/ 'newContactList' smart constructor.
data ContactList = ContactList'
  { -- | The name of the contact list.
    contactListName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp noting the last time the contact list was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactListName', 'contactList_contactListName' - The name of the contact list.
--
-- 'lastUpdatedTimestamp', 'contactList_lastUpdatedTimestamp' - A timestamp noting the last time the contact list was updated.
newContactList ::
  ContactList
newContactList =
  ContactList'
    { contactListName = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing
    }

-- | The name of the contact list.
contactList_contactListName :: Lens.Lens' ContactList (Prelude.Maybe Prelude.Text)
contactList_contactListName = Lens.lens (\ContactList' {contactListName} -> contactListName) (\s@ContactList' {} a -> s {contactListName = a} :: ContactList)

-- | A timestamp noting the last time the contact list was updated.
contactList_lastUpdatedTimestamp :: Lens.Lens' ContactList (Prelude.Maybe Prelude.UTCTime)
contactList_lastUpdatedTimestamp = Lens.lens (\ContactList' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ContactList' {} a -> s {lastUpdatedTimestamp = a} :: ContactList) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ContactList where
  parseJSON =
    Data.withObject
      "ContactList"
      ( \x ->
          ContactList'
            Prelude.<$> (x Data..:? "ContactListName")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
      )

instance Prelude.Hashable ContactList where
  hashWithSalt _salt ContactList' {..} =
    _salt
      `Prelude.hashWithSalt` contactListName
      `Prelude.hashWithSalt` lastUpdatedTimestamp

instance Prelude.NFData ContactList where
  rnf ContactList' {..} =
    Prelude.rnf contactListName
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
