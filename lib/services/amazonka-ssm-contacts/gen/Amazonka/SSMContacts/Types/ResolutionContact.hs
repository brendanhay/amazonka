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
-- Module      : Amazonka.SSMContacts.Types.ResolutionContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.ResolutionContact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.ContactType

-- | Information about the engagement resolution steps. The resolution starts
-- from the first contact, which can be an escalation plan, then resolves
-- to an on-call rotation, and finally to a personal contact.
--
-- The @ResolutionContact@ structure describes the information for each
-- node or step in that process. It contains information about different
-- contact types, such as the escalation, rotation, and personal contacts.
--
-- /See:/ 'newResolutionContact' smart constructor.
data ResolutionContact = ResolutionContact'
  { -- | The stage in the escalation plan that resolves to this contact.
    stageIndex :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of a contact in the engagement resolution
    -- process.
    contactArn :: Prelude.Text,
    -- | The type of contact for a resolution step.
    type' :: ContactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolutionContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageIndex', 'resolutionContact_stageIndex' - The stage in the escalation plan that resolves to this contact.
--
-- 'contactArn', 'resolutionContact_contactArn' - The Amazon Resource Name (ARN) of a contact in the engagement resolution
-- process.
--
-- 'type'', 'resolutionContact_type' - The type of contact for a resolution step.
newResolutionContact ::
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'type''
  ContactType ->
  ResolutionContact
newResolutionContact pContactArn_ pType_ =
  ResolutionContact'
    { stageIndex = Prelude.Nothing,
      contactArn = pContactArn_,
      type' = pType_
    }

-- | The stage in the escalation plan that resolves to this contact.
resolutionContact_stageIndex :: Lens.Lens' ResolutionContact (Prelude.Maybe Prelude.Natural)
resolutionContact_stageIndex = Lens.lens (\ResolutionContact' {stageIndex} -> stageIndex) (\s@ResolutionContact' {} a -> s {stageIndex = a} :: ResolutionContact)

-- | The Amazon Resource Name (ARN) of a contact in the engagement resolution
-- process.
resolutionContact_contactArn :: Lens.Lens' ResolutionContact Prelude.Text
resolutionContact_contactArn = Lens.lens (\ResolutionContact' {contactArn} -> contactArn) (\s@ResolutionContact' {} a -> s {contactArn = a} :: ResolutionContact)

-- | The type of contact for a resolution step.
resolutionContact_type :: Lens.Lens' ResolutionContact ContactType
resolutionContact_type = Lens.lens (\ResolutionContact' {type'} -> type') (\s@ResolutionContact' {} a -> s {type' = a} :: ResolutionContact)

instance Data.FromJSON ResolutionContact where
  parseJSON =
    Data.withObject
      "ResolutionContact"
      ( \x ->
          ResolutionContact'
            Prelude.<$> (x Data..:? "StageIndex")
            Prelude.<*> (x Data..: "ContactArn")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable ResolutionContact where
  hashWithSalt _salt ResolutionContact' {..} =
    _salt
      `Prelude.hashWithSalt` stageIndex
      `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ResolutionContact where
  rnf ResolutionContact' {..} =
    Prelude.rnf stageIndex
      `Prelude.seq` Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf type'
