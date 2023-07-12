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
-- Module      : Amazonka.Kendra.Types.EntityDisplayData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.EntityDisplayData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the user entity.
--
-- /See:/ 'newEntityDisplayData' smart constructor.
data EntityDisplayData = EntityDisplayData'
  { -- | The first name of the user.
    firstName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the group.
    groupName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The user name of the user.
    identifiedUserName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The last name of the user.
    lastName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the user.
    userName :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityDisplayData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firstName', 'entityDisplayData_firstName' - The first name of the user.
--
-- 'groupName', 'entityDisplayData_groupName' - The name of the group.
--
-- 'identifiedUserName', 'entityDisplayData_identifiedUserName' - The user name of the user.
--
-- 'lastName', 'entityDisplayData_lastName' - The last name of the user.
--
-- 'userName', 'entityDisplayData_userName' - The name of the user.
newEntityDisplayData ::
  EntityDisplayData
newEntityDisplayData =
  EntityDisplayData'
    { firstName = Prelude.Nothing,
      groupName = Prelude.Nothing,
      identifiedUserName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | The first name of the user.
entityDisplayData_firstName :: Lens.Lens' EntityDisplayData (Prelude.Maybe Prelude.Text)
entityDisplayData_firstName = Lens.lens (\EntityDisplayData' {firstName} -> firstName) (\s@EntityDisplayData' {} a -> s {firstName = a} :: EntityDisplayData) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the group.
entityDisplayData_groupName :: Lens.Lens' EntityDisplayData (Prelude.Maybe Prelude.Text)
entityDisplayData_groupName = Lens.lens (\EntityDisplayData' {groupName} -> groupName) (\s@EntityDisplayData' {} a -> s {groupName = a} :: EntityDisplayData) Prelude.. Lens.mapping Data._Sensitive

-- | The user name of the user.
entityDisplayData_identifiedUserName :: Lens.Lens' EntityDisplayData (Prelude.Maybe Prelude.Text)
entityDisplayData_identifiedUserName = Lens.lens (\EntityDisplayData' {identifiedUserName} -> identifiedUserName) (\s@EntityDisplayData' {} a -> s {identifiedUserName = a} :: EntityDisplayData) Prelude.. Lens.mapping Data._Sensitive

-- | The last name of the user.
entityDisplayData_lastName :: Lens.Lens' EntityDisplayData (Prelude.Maybe Prelude.Text)
entityDisplayData_lastName = Lens.lens (\EntityDisplayData' {lastName} -> lastName) (\s@EntityDisplayData' {} a -> s {lastName = a} :: EntityDisplayData) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the user.
entityDisplayData_userName :: Lens.Lens' EntityDisplayData (Prelude.Maybe Prelude.Text)
entityDisplayData_userName = Lens.lens (\EntityDisplayData' {userName} -> userName) (\s@EntityDisplayData' {} a -> s {userName = a} :: EntityDisplayData) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON EntityDisplayData where
  parseJSON =
    Data.withObject
      "EntityDisplayData"
      ( \x ->
          EntityDisplayData'
            Prelude.<$> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "GroupName")
            Prelude.<*> (x Data..:? "IdentifiedUserName")
            Prelude.<*> (x Data..:? "LastName")
            Prelude.<*> (x Data..:? "UserName")
      )

instance Prelude.Hashable EntityDisplayData where
  hashWithSalt _salt EntityDisplayData' {..} =
    _salt
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` identifiedUserName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` userName

instance Prelude.NFData EntityDisplayData where
  rnf EntityDisplayData' {..} =
    Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf identifiedUserName
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf userName
