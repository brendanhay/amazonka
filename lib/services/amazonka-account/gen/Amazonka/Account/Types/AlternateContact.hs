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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Types.AlternateContact where

import Amazonka.Account.Types.AlternateContactType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the details of an alternate contact associated
-- with an Amazon Web Services account
--
-- /See:/ 'newAlternateContact' smart constructor.
data AlternateContact = AlternateContact'
  { -- | The type of alternate contact.
    alternateContactType :: Prelude.Maybe AlternateContactType,
    -- | The email address associated with this alternate contact.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name associated with this alternate contact.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The phone number associated with this alternate contact.
    phoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The title associated with this alternate contact.
    title :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
-- 'alternateContactType', 'alternateContact_alternateContactType' - The type of alternate contact.
--
-- 'emailAddress', 'alternateContact_emailAddress' - The email address associated with this alternate contact.
--
-- 'name', 'alternateContact_name' - The name associated with this alternate contact.
--
-- 'phoneNumber', 'alternateContact_phoneNumber' - The phone number associated with this alternate contact.
--
-- 'title', 'alternateContact_title' - The title associated with this alternate contact.
newAlternateContact ::
  AlternateContact
newAlternateContact =
  AlternateContact'
    { alternateContactType =
        Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      name = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | The type of alternate contact.
alternateContact_alternateContactType :: Lens.Lens' AlternateContact (Prelude.Maybe AlternateContactType)
alternateContact_alternateContactType = Lens.lens (\AlternateContact' {alternateContactType} -> alternateContactType) (\s@AlternateContact' {} a -> s {alternateContactType = a} :: AlternateContact)

-- | The email address associated with this alternate contact.
alternateContact_emailAddress :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_emailAddress = Lens.lens (\AlternateContact' {emailAddress} -> emailAddress) (\s@AlternateContact' {} a -> s {emailAddress = a} :: AlternateContact) Prelude.. Lens.mapping Data._Sensitive

-- | The name associated with this alternate contact.
alternateContact_name :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_name = Lens.lens (\AlternateContact' {name} -> name) (\s@AlternateContact' {} a -> s {name = a} :: AlternateContact) Prelude.. Lens.mapping Data._Sensitive

-- | The phone number associated with this alternate contact.
alternateContact_phoneNumber :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_phoneNumber = Lens.lens (\AlternateContact' {phoneNumber} -> phoneNumber) (\s@AlternateContact' {} a -> s {phoneNumber = a} :: AlternateContact) Prelude.. Lens.mapping Data._Sensitive

-- | The title associated with this alternate contact.
alternateContact_title :: Lens.Lens' AlternateContact (Prelude.Maybe Prelude.Text)
alternateContact_title = Lens.lens (\AlternateContact' {title} -> title) (\s@AlternateContact' {} a -> s {title = a} :: AlternateContact) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON AlternateContact where
  parseJSON =
    Data.withObject
      "AlternateContact"
      ( \x ->
          AlternateContact'
            Prelude.<$> (x Data..:? "AlternateContactType")
            Prelude.<*> (x Data..:? "EmailAddress")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PhoneNumber")
            Prelude.<*> (x Data..:? "Title")
      )

instance Prelude.Hashable AlternateContact where
  hashWithSalt _salt AlternateContact' {..} =
    _salt `Prelude.hashWithSalt` alternateContactType
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` title

instance Prelude.NFData AlternateContact where
  rnf AlternateContact' {..} =
    Prelude.rnf alternateContactType
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf title
