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
-- Module      : Amazonka.IdentityStore.Types.Name
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.Name where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The full name of the user.
--
-- /See:/ 'newName' smart constructor.
data Name = Name'
  { -- | The family name of the user.
    familyName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A string containing a formatted version of the name for display.
    formatted :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The given name of the user.
    givenName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The honorific prefix of the user. For example, \"Dr.\"
    honorificPrefix :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The honorific suffix of the user. For example, \"M.D.\"
    honorificSuffix :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The middle name of the user.
    middleName :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Name' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'familyName', 'name_familyName' - The family name of the user.
--
-- 'formatted', 'name_formatted' - A string containing a formatted version of the name for display.
--
-- 'givenName', 'name_givenName' - The given name of the user.
--
-- 'honorificPrefix', 'name_honorificPrefix' - The honorific prefix of the user. For example, \"Dr.\"
--
-- 'honorificSuffix', 'name_honorificSuffix' - The honorific suffix of the user. For example, \"M.D.\"
--
-- 'middleName', 'name_middleName' - The middle name of the user.
newName ::
  Name
newName =
  Name'
    { familyName = Prelude.Nothing,
      formatted = Prelude.Nothing,
      givenName = Prelude.Nothing,
      honorificPrefix = Prelude.Nothing,
      honorificSuffix = Prelude.Nothing,
      middleName = Prelude.Nothing
    }

-- | The family name of the user.
name_familyName :: Lens.Lens' Name (Prelude.Maybe Prelude.Text)
name_familyName = Lens.lens (\Name' {familyName} -> familyName) (\s@Name' {} a -> s {familyName = a} :: Name) Prelude.. Lens.mapping Data._Sensitive

-- | A string containing a formatted version of the name for display.
name_formatted :: Lens.Lens' Name (Prelude.Maybe Prelude.Text)
name_formatted = Lens.lens (\Name' {formatted} -> formatted) (\s@Name' {} a -> s {formatted = a} :: Name) Prelude.. Lens.mapping Data._Sensitive

-- | The given name of the user.
name_givenName :: Lens.Lens' Name (Prelude.Maybe Prelude.Text)
name_givenName = Lens.lens (\Name' {givenName} -> givenName) (\s@Name' {} a -> s {givenName = a} :: Name) Prelude.. Lens.mapping Data._Sensitive

-- | The honorific prefix of the user. For example, \"Dr.\"
name_honorificPrefix :: Lens.Lens' Name (Prelude.Maybe Prelude.Text)
name_honorificPrefix = Lens.lens (\Name' {honorificPrefix} -> honorificPrefix) (\s@Name' {} a -> s {honorificPrefix = a} :: Name) Prelude.. Lens.mapping Data._Sensitive

-- | The honorific suffix of the user. For example, \"M.D.\"
name_honorificSuffix :: Lens.Lens' Name (Prelude.Maybe Prelude.Text)
name_honorificSuffix = Lens.lens (\Name' {honorificSuffix} -> honorificSuffix) (\s@Name' {} a -> s {honorificSuffix = a} :: Name) Prelude.. Lens.mapping Data._Sensitive

-- | The middle name of the user.
name_middleName :: Lens.Lens' Name (Prelude.Maybe Prelude.Text)
name_middleName = Lens.lens (\Name' {middleName} -> middleName) (\s@Name' {} a -> s {middleName = a} :: Name) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Name where
  parseJSON =
    Data.withObject
      "Name"
      ( \x ->
          Name'
            Prelude.<$> (x Data..:? "FamilyName")
            Prelude.<*> (x Data..:? "Formatted")
            Prelude.<*> (x Data..:? "GivenName")
            Prelude.<*> (x Data..:? "HonorificPrefix")
            Prelude.<*> (x Data..:? "HonorificSuffix")
            Prelude.<*> (x Data..:? "MiddleName")
      )

instance Prelude.Hashable Name where
  hashWithSalt _salt Name' {..} =
    _salt `Prelude.hashWithSalt` familyName
      `Prelude.hashWithSalt` formatted
      `Prelude.hashWithSalt` givenName
      `Prelude.hashWithSalt` honorificPrefix
      `Prelude.hashWithSalt` honorificSuffix
      `Prelude.hashWithSalt` middleName

instance Prelude.NFData Name where
  rnf Name' {..} =
    Prelude.rnf familyName
      `Prelude.seq` Prelude.rnf formatted
      `Prelude.seq` Prelude.rnf givenName
      `Prelude.seq` Prelude.rnf honorificPrefix
      `Prelude.seq` Prelude.rnf honorificSuffix
      `Prelude.seq` Prelude.rnf middleName

instance Data.ToJSON Name where
  toJSON Name' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FamilyName" Data..=) Prelude.<$> familyName,
            ("Formatted" Data..=) Prelude.<$> formatted,
            ("GivenName" Data..=) Prelude.<$> givenName,
            ("HonorificPrefix" Data..=)
              Prelude.<$> honorificPrefix,
            ("HonorificSuffix" Data..=)
              Prelude.<$> honorificSuffix,
            ("MiddleName" Data..=) Prelude.<$> middleName
          ]
      )
