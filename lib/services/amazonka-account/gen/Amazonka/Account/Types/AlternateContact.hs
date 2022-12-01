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
-- Module      : Amazonka.Account.Types.AlternateContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Types.AlternateContact where

import Amazonka.Account.Types.AlternateContactType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the details of an alternate contact associated
-- with an Amazon Web Services account
--
-- /See:/ 'newAlternateContact' smart constructor.
data AlternateContact = AlternateContact'
  { -- | The name associated with this alternate contact.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The type of alternate contact.
    alternateContactType :: Prelude.Maybe AlternateContactType,
    -- | The title associated with this alternate contact.
    title :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The phone number associated with this alternate contact.
    phoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The email address associated with this alternate contact.
    emailAddress :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlternateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'alternateContact_name' - The name associated with this alternate contact.
--
-- 'alternateContactType', 'alternateContact_alternateContactType' - The type of alternate contact.
--
-- 'title', 'alternateContact_title' - The title associated with this alternate contact.
--
-- 'phoneNumber', 'alternateContact_phoneNumber' - The phone number associated with this alternate contact.
--
-- 'emailAddress', 'alternateContact_emailAddress' - The email address associated with this alternate contact.
newAlternateContact ::
  AlternateContact
newAlternateContact =
  AlternateContact'
    { name = Prelude.Nothing,
      alternateContactType = Prelude.Nothing,
      title = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      emailAddress = Prelude.Nothing
    }

-- | The name associated with this alternate contact.
alternateContact_name :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_name = Lens.lens (\AlternateContact' {name} -> name) (\s@AlternateContact' {} a -> s {name = a} :: AlternateContact) Prelude.. Lens.mapping Core._Sensitive

-- | The type of alternate contact.
alternateContact_alternateContactType :: Lens.Lens' AlternateContact (Prelude.Maybe AlternateContactType)
alternateContact_alternateContactType = Lens.lens (\AlternateContact' {alternateContactType} -> alternateContactType) (\s@AlternateContact' {} a -> s {alternateContactType = a} :: AlternateContact)

-- | The title associated with this alternate contact.
alternateContact_title :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_title = Lens.lens (\AlternateContact' {title} -> title) (\s@AlternateContact' {} a -> s {title = a} :: AlternateContact) Prelude.. Lens.mapping Core._Sensitive

-- | The phone number associated with this alternate contact.
alternateContact_phoneNumber :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_phoneNumber = Lens.lens (\AlternateContact' {phoneNumber} -> phoneNumber) (\s@AlternateContact' {} a -> s {phoneNumber = a} :: AlternateContact) Prelude.. Lens.mapping Core._Sensitive

-- | The email address associated with this alternate contact.
alternateContact_emailAddress :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_emailAddress = Lens.lens (\AlternateContact' {emailAddress} -> emailAddress) (\s@AlternateContact' {} a -> s {emailAddress = a} :: AlternateContact) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON AlternateContact where
  parseJSON =
    Core.withObject
      "AlternateContact"
      ( \x ->
          AlternateContact'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "AlternateContactType")
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "PhoneNumber")
            Prelude.<*> (x Core..:? "EmailAddress")
      )

instance Prelude.Hashable AlternateContact where
  hashWithSalt _salt AlternateContact' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` alternateContactType
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData AlternateContact where
  rnf AlternateContact' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf alternateContactType
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf emailAddress
