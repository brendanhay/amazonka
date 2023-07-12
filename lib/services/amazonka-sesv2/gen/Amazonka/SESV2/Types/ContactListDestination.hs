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
-- Module      : Amazonka.SESV2.Types.ContactListDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ContactListDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.ContactListImportAction

-- | An object that contains details about the action of a contact list.
--
-- /See:/ 'newContactListDestination' smart constructor.
data ContactListDestination = ContactListDestination'
  { -- | The name of the contact list.
    contactListName :: Prelude.Text,
    -- | >The type of action to perform on the addresses. The following are the
    -- possible values:
    --
    -- -   PUT: add the addresses to the contact list. If the record already
    --     exists, it will override it with the new value.
    --
    -- -   DELETE: remove the addresses from the contact list.
    contactListImportAction :: ContactListImportAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactListDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactListName', 'contactListDestination_contactListName' - The name of the contact list.
--
-- 'contactListImportAction', 'contactListDestination_contactListImportAction' - >The type of action to perform on the addresses. The following are the
-- possible values:
--
-- -   PUT: add the addresses to the contact list. If the record already
--     exists, it will override it with the new value.
--
-- -   DELETE: remove the addresses from the contact list.
newContactListDestination ::
  -- | 'contactListName'
  Prelude.Text ->
  -- | 'contactListImportAction'
  ContactListImportAction ->
  ContactListDestination
newContactListDestination
  pContactListName_
  pContactListImportAction_ =
    ContactListDestination'
      { contactListName =
          pContactListName_,
        contactListImportAction = pContactListImportAction_
      }

-- | The name of the contact list.
contactListDestination_contactListName :: Lens.Lens' ContactListDestination Prelude.Text
contactListDestination_contactListName = Lens.lens (\ContactListDestination' {contactListName} -> contactListName) (\s@ContactListDestination' {} a -> s {contactListName = a} :: ContactListDestination)

-- | >The type of action to perform on the addresses. The following are the
-- possible values:
--
-- -   PUT: add the addresses to the contact list. If the record already
--     exists, it will override it with the new value.
--
-- -   DELETE: remove the addresses from the contact list.
contactListDestination_contactListImportAction :: Lens.Lens' ContactListDestination ContactListImportAction
contactListDestination_contactListImportAction = Lens.lens (\ContactListDestination' {contactListImportAction} -> contactListImportAction) (\s@ContactListDestination' {} a -> s {contactListImportAction = a} :: ContactListDestination)

instance Data.FromJSON ContactListDestination where
  parseJSON =
    Data.withObject
      "ContactListDestination"
      ( \x ->
          ContactListDestination'
            Prelude.<$> (x Data..: "ContactListName")
            Prelude.<*> (x Data..: "ContactListImportAction")
      )

instance Prelude.Hashable ContactListDestination where
  hashWithSalt _salt ContactListDestination' {..} =
    _salt
      `Prelude.hashWithSalt` contactListName
      `Prelude.hashWithSalt` contactListImportAction

instance Prelude.NFData ContactListDestination where
  rnf ContactListDestination' {..} =
    Prelude.rnf contactListName
      `Prelude.seq` Prelude.rnf contactListImportAction

instance Data.ToJSON ContactListDestination where
  toJSON ContactListDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContactListName" Data..= contactListName),
            Prelude.Just
              ( "ContactListImportAction"
                  Data..= contactListImportAction
              )
          ]
      )
