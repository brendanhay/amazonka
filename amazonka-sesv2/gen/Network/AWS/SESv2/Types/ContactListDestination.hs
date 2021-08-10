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
-- Module      : Network.AWS.SESv2.Types.ContactListDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.ContactListDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.ContactListImportAction

-- | An object that contains details about the action of a contact list.
--
-- /See:/ 'newContactListDestination' smart constructor.
data ContactListDestination = ContactListDestination'
  { -- | The name of the contact list.
    contactListName :: Prelude.Text,
    -- | >The type of action that you want to perform on the addresses.
    -- Acceptable values:
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
-- 'contactListImportAction', 'contactListDestination_contactListImportAction' - >The type of action that you want to perform on the addresses.
-- Acceptable values:
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

-- | >The type of action that you want to perform on the addresses.
-- Acceptable values:
--
-- -   PUT: add the addresses to the contact list. If the record already
--     exists, it will override it with the new value.
--
-- -   DELETE: remove the addresses from the contact list.
contactListDestination_contactListImportAction :: Lens.Lens' ContactListDestination ContactListImportAction
contactListDestination_contactListImportAction = Lens.lens (\ContactListDestination' {contactListImportAction} -> contactListImportAction) (\s@ContactListDestination' {} a -> s {contactListImportAction = a} :: ContactListDestination)

instance Core.FromJSON ContactListDestination where
  parseJSON =
    Core.withObject
      "ContactListDestination"
      ( \x ->
          ContactListDestination'
            Prelude.<$> (x Core..: "ContactListName")
            Prelude.<*> (x Core..: "ContactListImportAction")
      )

instance Prelude.Hashable ContactListDestination

instance Prelude.NFData ContactListDestination

instance Core.ToJSON ContactListDestination where
  toJSON ContactListDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContactListName" Core..= contactListName),
            Prelude.Just
              ( "ContactListImportAction"
                  Core..= contactListImportAction
              )
          ]
      )
