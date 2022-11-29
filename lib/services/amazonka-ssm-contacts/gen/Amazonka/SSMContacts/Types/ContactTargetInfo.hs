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
-- Module      : Amazonka.SSMContacts.Types.ContactTargetInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.ContactTargetInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The contact that Incident Manager is engaging during an incident.
--
-- /See:/ 'newContactTargetInfo' smart constructor.
data ContactTargetInfo = ContactTargetInfo'
  { -- | The Amazon Resource Name (ARN) of the contact.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value determining if the contact\'s acknowledgement stops the
    -- progress of stages in the plan.
    isEssential :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactTargetInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'contactTargetInfo_contactId' - The Amazon Resource Name (ARN) of the contact.
--
-- 'isEssential', 'contactTargetInfo_isEssential' - A Boolean value determining if the contact\'s acknowledgement stops the
-- progress of stages in the plan.
newContactTargetInfo ::
  -- | 'isEssential'
  Prelude.Bool ->
  ContactTargetInfo
newContactTargetInfo pIsEssential_ =
  ContactTargetInfo'
    { contactId = Prelude.Nothing,
      isEssential = pIsEssential_
    }

-- | The Amazon Resource Name (ARN) of the contact.
contactTargetInfo_contactId :: Lens.Lens' ContactTargetInfo (Prelude.Maybe Prelude.Text)
contactTargetInfo_contactId = Lens.lens (\ContactTargetInfo' {contactId} -> contactId) (\s@ContactTargetInfo' {} a -> s {contactId = a} :: ContactTargetInfo)

-- | A Boolean value determining if the contact\'s acknowledgement stops the
-- progress of stages in the plan.
contactTargetInfo_isEssential :: Lens.Lens' ContactTargetInfo Prelude.Bool
contactTargetInfo_isEssential = Lens.lens (\ContactTargetInfo' {isEssential} -> isEssential) (\s@ContactTargetInfo' {} a -> s {isEssential = a} :: ContactTargetInfo)

instance Core.FromJSON ContactTargetInfo where
  parseJSON =
    Core.withObject
      "ContactTargetInfo"
      ( \x ->
          ContactTargetInfo'
            Prelude.<$> (x Core..:? "ContactId")
            Prelude.<*> (x Core..: "IsEssential")
      )

instance Prelude.Hashable ContactTargetInfo where
  hashWithSalt _salt ContactTargetInfo' {..} =
    _salt `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` isEssential

instance Prelude.NFData ContactTargetInfo where
  rnf ContactTargetInfo' {..} =
    Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf isEssential

instance Core.ToJSON ContactTargetInfo where
  toJSON ContactTargetInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContactId" Core..=) Prelude.<$> contactId,
            Prelude.Just ("IsEssential" Core..= isEssential)
          ]
      )
