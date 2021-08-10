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
-- Module      : Network.AWS.SESv2.Types.ContactList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.ContactList where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list that contains contacts that have subscribed to a particular topic
-- or topics.
--
-- /See:/ 'newContactList' smart constructor.
data ContactList = ContactList'
  { -- | A timestamp noting the last time the contact list was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the contact list.
    contactListName :: Prelude.Maybe Prelude.Text
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
-- 'lastUpdatedTimestamp', 'contactList_lastUpdatedTimestamp' - A timestamp noting the last time the contact list was updated.
--
-- 'contactListName', 'contactList_contactListName' - The name of the contact list.
newContactList ::
  ContactList
newContactList =
  ContactList'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      contactListName = Prelude.Nothing
    }

-- | A timestamp noting the last time the contact list was updated.
contactList_lastUpdatedTimestamp :: Lens.Lens' ContactList (Prelude.Maybe Prelude.UTCTime)
contactList_lastUpdatedTimestamp = Lens.lens (\ContactList' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ContactList' {} a -> s {lastUpdatedTimestamp = a} :: ContactList) Prelude.. Lens.mapping Core._Time

-- | The name of the contact list.
contactList_contactListName :: Lens.Lens' ContactList (Prelude.Maybe Prelude.Text)
contactList_contactListName = Lens.lens (\ContactList' {contactListName} -> contactListName) (\s@ContactList' {} a -> s {contactListName = a} :: ContactList)

instance Core.FromJSON ContactList where
  parseJSON =
    Core.withObject
      "ContactList"
      ( \x ->
          ContactList'
            Prelude.<$> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "ContactListName")
      )

instance Prelude.Hashable ContactList

instance Prelude.NFData ContactList
